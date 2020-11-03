package cache

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest.internal.LineCoverageAnnotation
import chiseltest._
import chisel3.experimental.BundleLiterals._
import firrtl.stage.RunFirrtlTransformAnnotation
import chiseltest.ChiselScalatestTester
import device.AXI4RAM
import freechips.rocketchip.amba.axi4.AXI4UserYanker
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBuffer, TLCacheCork, TLToAXI4, TLXbar}
import org.scalatest.{FlatSpec, Matchers}
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import utils.{DebugIdentityNode, HoldUnless, XSDebug}
import xiangshan.HasXSLog
import xiangshan._
import xiangshan.backend._
import xiangshan.backend.exu._
import xiangshan.backend.dispatch._
import xiangshan.backend.roq._
import xiangshan.backend.regfile._
import xiangshan.backend.issue._
import xiangshan.backend.rename._
import xiangshan.mem._
import xiangshan.cache.{DCache, DCacheLineReq, DCacheWordReq, MemoryOpConstants, PTW}
import xiangshan.testutils.AddSinks
import xstransforms.PrintModuleName

import scala.util.Random

case class L2CacheSystemTestParams
(
  ways: Int = 4,
  banks: Int = 1,
  capacityKB: Int = 4,
  blockBytes: Int = 64,
  beatBytes: Int = 32,
  writeBytes: Int = 8
) {
  require(blockBytes >= beatBytes)
}

case object L2CacheSystemTestKey extends Field[L2CacheSystemTestParams]

class CacheSystemTestIO extends XSBundle{
  val testVec = Vec(RenameWidth, Flipped(DecoupledIO(new CfCtrl)))
}

class FakeBackend extends XSModule{
  val io = IO(new Bundle {
    val cfInput = Vec(RenameWidth, Flipped(DecoupledIO(new CfCtrl)))
    val mem = Flipped(new MemToBackendIO)
  })

  val aluExeUnits =Array.tabulate(exuParameters.AluCnt)(_ => Module(new AluExeUnit))
  val jmpExeUnit = Module(new JmpExeUnit)
  val mulExeUnits = Array.tabulate(exuParameters.MulCnt)(_ => Module(new MulExeUnit))
  val mduExeUnits = Array.tabulate(exuParameters.MduCnt)(_ => Module(new MulDivExeUnit))
  // val fmacExeUnits = Array.tabulate(exuParameters.FmacCnt)(_ => Module(new Fmac))
  // val fmiscExeUnits = Array.tabulate(exuParameters.FmiscCnt)(_ => Module(new Fmisc))
  // val fmiscDivSqrtExeUnits = Array.tabulate(exuParameters.FmiscDivSqrtCnt)(_ => Module(new FmiscDivSqrt))
  val exeUnits = jmpExeUnit +: (aluExeUnits ++ mulExeUnits ++ mduExeUnits)
  exeUnits.foreach(_.io.exception := DontCare)
  exeUnits.foreach(_.io.dmem := DontCare)
  exeUnits.foreach(_.io.mcommit := DontCare)

  val dispatch = Module(new Dispatch)
  val rename = Module(new Rename)
  val roq = Module(new Roq)
  val intRf = Module(new Regfile(
    numReadPorts = NRIntReadPorts,
    numWirtePorts = NRIntWritePorts,
    hasZero = true
  ))
  val fpRf = Module(new Regfile(
    numReadPorts = NRFpReadPorts,
    numWirtePorts = NRFpWritePorts,
    hasZero = false
  ))
  val memRf = Module(new Regfile(
    numReadPorts = 2*exuParameters.StuCnt + exuParameters.LduCnt,
    numWirtePorts = NRIntWritePorts,
    hasZero = true,
    isMemRf = true
  ))
  
  val memConfigs =
    Seq.fill(exuParameters.LduCnt)(Exu.ldExeUnitCfg) ++
    Seq.fill(exuParameters.StuCnt)(Exu.stExeUnitCfg)

  val exuConfigs = exeUnits.map(_.config) ++ memConfigs

  val exeWbReqs = exeUnits.map(_.io.out) ++ io.mem.ldout ++ io.mem.stout

  def needWakeup(cfg: ExuConfig): Boolean =
    (cfg.readIntRf && cfg.writeIntRf) || (cfg.readFpRf && cfg.writeFpRf)

  def needData(a: ExuConfig, b: ExuConfig): Boolean =
    (a.readIntRf && b.writeIntRf) || (a.readFpRf && b.writeFpRf)

  // backend redirect, flush pipeline
  val redirect = Mux(
    roq.io.redirect.valid,
    roq.io.redirect,
    io.mem.replayAll
  )
  
  val reservedStations = exeUnits.
    zipWithIndex.
    map({ case (exu, i) =>

      val cfg = exu.config

      val wakeUpDateVec = exuConfigs.zip(exeWbReqs).filter(x => needData(cfg, x._1)).map(_._2)
      val bypassCnt = exuConfigs.count(c => c.enableBypass && needData(cfg, c))

      println(s"exu:${cfg.name} wakeupCnt:${wakeUpDateVec.length} bypassCnt:$bypassCnt")

      val rs = Module(new ReservationStation(
        cfg, wakeUpDateVec.length, bypassCnt, cfg.enableBypass, false
      ))
      rs.io.redirect <> redirect
      rs.io.numExist <> dispatch.io.numExist(i)
      rs.io.enqCtrl <> dispatch.io.enqIQCtrl(i)
      rs.io.enqData <> dispatch.io.enqIQData(i)
      for(
        (wakeUpPort, exuOut) <-
        rs.io.wakeUpPorts.zip(wakeUpDateVec)
      ){
        wakeUpPort.bits := exuOut.bits
        wakeUpPort.valid := exuOut.valid
      }

      exu.io.in <> rs.io.deq
      exu.io.redirect <> redirect
      rs
    })

  for( rs <- reservedStations){
    rs.io.bypassUops <> reservedStations.
      filter(x => x.enableBypass && needData(rs.exuCfg, x.exuCfg)).
      map(_.io.selectedUop)

    val bypassDataVec = exuConfigs.zip(exeWbReqs).
      filter(x => x._1.enableBypass && needData(rs.exuCfg, x._1)).map(_._2)

    for(i <- bypassDataVec.indices){
      rs.io.bypassData(i).valid := bypassDataVec(i).valid
      rs.io.bypassData(i).bits := bypassDataVec(i).bits
    }
  }

  val issueQueues = exuConfigs.
    zipWithIndex.
    takeRight(exuParameters.LduCnt + exuParameters.StuCnt).
    map({case (cfg, i) =>
      val wakeUpDateVec = exuConfigs.zip(exeWbReqs).filter(x => needData(cfg, x._1)).map(_._2)
      val bypassUopVec = reservedStations.
        filter(r => r.exuCfg.enableBypass && needData(cfg, r.exuCfg)).map(_.io.selectedUop)
      val bypassDataVec = exuConfigs.zip(exeWbReqs).
        filter(x => x._1.enableBypass && needData(cfg, x._1)).map(_._2)

      val iq = Module(new IssueQueue(
        cfg, wakeUpDateVec.length, bypassUopVec.length
      ))
      println(s"exu:${cfg.name} wakeupCnt:${wakeUpDateVec.length} bypassCnt:${bypassUopVec.length}")
      iq.io.redirect <> redirect
      iq.io.tlbFeedback := io.mem.tlbFeedback(i - exuParameters.ExuCnt + exuParameters.LduCnt + exuParameters.StuCnt)
      iq.io.enq <> dispatch.io.enqIQCtrl(i)
      dispatch.io.numExist(i) := iq.io.numExist
      for(
        (wakeUpPort, exuOut) <-
        iq.io.wakeUpPorts.zip(wakeUpDateVec)
      ){
        wakeUpPort.bits := exuOut.bits
        wakeUpPort.valid := exuOut.fire() // data after arbit
      }
      iq.io.bypassUops <> bypassUopVec
      for(i <- bypassDataVec.indices){
        iq.io.bypassData(i).valid := bypassDataVec(i).valid
        iq.io.bypassData(i).bits := bypassDataVec(i).bits
      }
      iq
    })


  //module connection
  io.mem.commits <> roq.io.commits
  io.mem.roqDeqPtr := roq.io.roqDeqPtr
  io.mem.ldin <> issueQueues.filter(_.exuCfg == Exu.ldExeUnitCfg).map(_.io.deq)
  io.mem.stin <> issueQueues.filter(_.exuCfg == Exu.stExeUnitCfg).map(_.io.deq)

  rename.io.in <> io.cfInput
  rename.io.redirect <> redirect
  rename.io.roqCommits <> roq.io.commits
  rename.io.intRfReadAddr <> dispatch.io.readIntRf.map(_.addr) ++ dispatch.io.intMemRegAddr
  rename.io.intPregRdy <> dispatch.io.intPregRdy ++ dispatch.io.intMemRegRdy
  rename.io.fpRfReadAddr <> dispatch.io.readFpRf.map(_.addr) ++ dispatch.io.fpMemRegAddr
  rename.io.fpPregRdy <> dispatch.io.fpPregRdy ++ dispatch.io.fpMemRegRdy
  rename.io.replayPregReq <> dispatch.io.replayPregReq
  dispatch.io.redirect <> redirect
  dispatch.io.fromRename <> rename.io.out

  roq.io.memRedirect <> io.mem.replayAll
  roq.io.brqRedirect <> DontCare
  roq.io.dp1Req <> dispatch.io.toRoq
  dispatch.io.roqIdxs <> roq.io.roqIdxs
  io.mem.dp1Req <> dispatch.io.toLsroq
  dispatch.io.lsIdxs <> io.mem.lsIdxs
  dispatch.io.dequeueRoqIndex.valid := roq.io.commitRoqIndex.valid || io.mem.oldestStore.valid
  // store writeback must be after commit roqIdx
  dispatch.io.dequeueRoqIndex.bits := Mux(io.mem.oldestStore.valid, io.mem.oldestStore.bits, roq.io.commitRoqIndex.bits)


  intRf.io.readPorts <> dispatch.io.readIntRf
  fpRf.io.readPorts <> dispatch.io.readFpRf ++ issueQueues.flatMap(_.io.readFpRf)
  memRf.io.readPorts <> issueQueues.flatMap(_.io.readIntRf)

  io.mem.redirect <> redirect

  val wbu = Module(new Wbu(exuConfigs))
  wbu.io.in <> exeWbReqs

  val wbIntResults = wbu.io.toIntRf
  val wbFpResults = wbu.io.toFpRf

  def exuOutToRfWrite(x: Valid[ExuOutput]): RfWritePort = {
    val rfWrite = Wire(new RfWritePort)
    rfWrite.wen := x.valid
    rfWrite.addr := x.bits.uop.pdest
    rfWrite.data := x.bits.data
    rfWrite
  }
  val intRfWrite = wbIntResults.map(exuOutToRfWrite)
  intRf.io.writePorts <> intRfWrite
  memRf.io.writePorts <> intRfWrite
  fpRf.io.writePorts <> wbFpResults.map(exuOutToRfWrite)

  rename.io.wbIntResults <> wbIntResults
  rename.io.wbFpResults <> wbFpResults

  roq.io.exeWbResults.take(exeWbReqs.length).zip(wbu.io.toRoq).foreach(x => x._1 := x._2)
  roq.io.exeWbResults.last := DontCare
}

class CacheSystemTestTop()(implicit p: Parameters) extends LazyModule{
  // TODO: refactor these params
  val dcache = LazyModule(new DCache())
  val ptw = LazyModule(new PTW())
  
  val l2params = p(L2CacheSystemTestKey)

  private val l2 = LazyModule(new InclusiveCache(
    CacheParameters(
      level = 2,
      ways = 4,
      sets = 512 * 1024 / (64 * 4),
      blockBytes = 64,
      beatBytes = 32 // beatBytes = l1BusDataWidth / 8
    ),
    InclusiveCacheMicroParameters(
      writeBytes = 8
    )
  ))


  val axiRam = LazyModule(new AXI4RAM(
    AddressSet(0x0L, 0xffffffffffL),
    memByte = 128 * 1024 * 1024,
    useBlackBox = false
  ))

  private val xbar = TLXbar()

  xbar := TLBuffer() := DebugIdentityNode() := dcache.clientNode
  xbar := TLBuffer() := DebugIdentityNode() := ptw.node

  l2.node := xbar

  axiRam.node := AXI4UserYanker() := TLToAXI4() := TLBuffer() := TLCacheCork() := l2.node

  lazy val module = new LazyModuleImp(this) with HasXSLog with HasXSParameter
  {
    val io = IO(new CacheSystemTestIO)

    val backend = Module(new FakeBackend)
    val mem = Module(new Memend)

    val dcacheModule = dcache.module
    val ptwModule = ptw.module

    mem.io.backend   <> backend.io.mem
    backend.io.cfInput     <>  io.testVec

    ptwModule.io.tlb(0) <> mem.io.ptw
    ptwModule.io.tlb(1) <> DontCare

    dcacheModule.io.lsu.load    <> mem.io.loadUnitToDcacheVec
    dcacheModule.io.lsu.lsroq   <> mem.io.loadMiss
    dcacheModule.io.lsu.atomics <> mem.io.atomics
    dcacheModule.io.lsu.store   <> mem.io.sbufferToDcache
    mem.io.uncache <> DontCare
  }

}

class CacheSystemTestTopWrapper()(implicit p: Parameters) extends LazyModule {

  val testTop = LazyModule(new CacheSystemTestTop())

  lazy val module = new LazyModuleImp(this){
    val io = IO(new CacheSystemTestIO)

    AddSinks()

    io <> testTop.module.io
  }
}

class CacheSystemTest extends FlatSpec with ChiselScalatestTester with Matchers{

  top.Parameters.set(top.Parameters.debugParameters)

  val annos = Seq(
    VerilatorBackendAnnotation,
    LineCoverageAnnotation,
    RunFirrtlTransformAnnotation(new PrintModuleName)
  )

  it should "run" in {

    implicit val p = Parameters((site, up, here) => {
      case L2CacheSystemTestKey =>
        L2CacheSystemTestParams()
    })

     test(LazyModule(new CacheSystemTestTopWrapper()).module)
      .withAnnotations(annos){ c =>
        c.clock.step(100)
        //TODO: Test Vector Generation
        c.io.testVec <> DontCare

      }
  }
}
