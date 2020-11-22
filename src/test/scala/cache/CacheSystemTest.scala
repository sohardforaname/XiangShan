package cache

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest.internal.LineCoverageAnnotation
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.util.experimental.BoringUtils
import firrtl.stage.RunFirrtlTransformAnnotation
import chiseltest.ChiselScalatestTester
import device.AXI4RAM
import freechips.rocketchip.amba.axi4.AXI4UserYanker
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBuffer, TLCacheCork, TLToAXI4, TLXbar}
import org.scalatest.{FlatSpec, Matchers}
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import top.{DiffTestIO, TrapIO}
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

class CacheSystemTestIO extends XSBundle {
  val testVec = Vec(RenameWidth, Flipped(DecoupledIO(new CfCtrl)))
}

class FakeBackend extends Module with HasXSParameter {
  val io = IO(new Bundle {
    val cfInput = Vec(RenameWidth, Flipped(DecoupledIO(new CfCtrl)))
    val mem = Flipped(new MemToBackendIO)
  })

  val aluExeUnits = Array.tabulate(exuParameters.AluCnt)(_ => Module(new AluExeUnit))
  val exeUnits = aluExeUnits
  exeUnits.foreach(_.io <> DontCare)

  val rename = Module(new Rename)
  val dispatch = Module(new Dispatch)
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

  val reservedStations  = exuConfigs.zipWithIndex.map({ case (cfg, i) =>

    // NOTE: exu could have certern and uncertaion latency
    // but could not have multiple certern latency
    var certainLatency = -1
    if(cfg.hasCertainLatency) { certainLatency = cfg.latency.latencyVal.get }

    val writeBackedData = exuConfigs.zip(exeWbReqs).filter(x => x._1.hasCertainLatency && needData(cfg, x._1)).map(_._2.bits.data)
    val wakeupCnt = writeBackedData.length

    val extraListenPorts = exuConfigs
      .zip(exeWbReqs)
      .filter(x => x._1.hasUncertainlatency && needData(cfg, x._1))
      .map(_._2)
    val extraListenPortsCnt = extraListenPorts.length

    val feedback = (cfg == Exu.ldExeUnitCfg) || (cfg == Exu.stExeUnitCfg)

    println(s"${i}: exu:${cfg.name} wakeupCnt: ${wakeupCnt} extraListenPorts: ${extraListenPortsCnt} delay:${certainLatency} feedback:${feedback}")

    val rs = Module(new ReservationStationNew(cfg, wakeupCnt, extraListenPortsCnt, fixedDelay = certainLatency, feedback = feedback))

    rs.io.redirect <> redirect
    rs.io.numExist <> dispatch.io.numExist(i)
    rs.io.enqCtrl <> dispatch.io.enqIQCtrl(i)
    rs.io.enqData <> dispatch.io.enqIQData(i)

    rs.io.writeBackedData <> writeBackedData
    for((x, y) <- rs.io.extraListenPorts.zip(extraListenPorts)){
      x.valid := y.fire()
      x.bits := y.bits
    }

    cfg match {
      case Exu.ldExeUnitCfg =>
      case Exu.stExeUnitCfg =>
      case otherCfg =>
        exeUnits(i).io.in <> rs.io.deq
        exeUnits(i).io.redirect <> redirect
        rs.io.tlbFeedback := DontCare
    }

    rs
  })


  for(rs <- reservedStations){
    rs.io.broadcastedUops <> reservedStations.
      filter(x => x.exuCfg.hasCertainLatency && needData(rs.exuCfg, x.exuCfg)).
      map(_.io.selectedUop)
  }


  //module connection
  io.mem <> DontCare
  io.mem.commits <> roq.io.commits
  io.mem.roqDeqPtr := roq.io.roqDeqPtr
  io.mem.ldin <> reservedStations.filter(_.exuCfg == Exu.ldExeUnitCfg).map(_.io.deq)
  io.mem.stin <> reservedStations.filter(_.exuCfg == Exu.stExeUnitCfg).map(_.io.deq)

  io.mem.exceptionAddr.lsIdx.lsroqIdx := roq.io.exception.lsroqIdx
  io.mem.exceptionAddr.lsIdx.lqIdx := roq.io.exception.lqIdx
  io.mem.exceptionAddr.lsIdx.sqIdx := roq.io.exception.sqIdx
  io.mem.exceptionAddr.isStore := CommitType.lsInstIsStore(roq.io.exception.ctrl.commitType)

  io.mem.tlbFeedback <> reservedStations.filter(
    x => x.exuCfg == Exu.ldExeUnitCfg || x.exuCfg == Exu.stExeUnitCfg
  ).map(_.io.tlbFeedback)
  

  rename.io.in <> io.cfInput
  rename.io.redirect <> redirect
  rename.io.roqCommits <> roq.io.commits
  rename.io.intRfReadAddr <> dispatch.io.readIntRf.map(_.addr) ++ dispatch.io.memIntRf.map(_.addr)
  rename.io.intPregRdy <> dispatch.io.intPregRdy ++ dispatch.io.intMemRegRdy
  rename.io.fpRfReadAddr <> dispatch.io.readFpRf.map(_.addr) ++ dispatch.io.memFpRf.map(_.addr)
  rename.io.fpPregRdy <> dispatch.io.fpPregRdy ++ dispatch.io.fpMemRegRdy
  rename.io.replayPregReq <> dispatch.io.replayPregReq
  dispatch.io <> DontCare
  dispatch.io.redirect <> redirect
  dispatch.io.fromRename <> rename.io.out

  roq.io <> DontCare
  roq.io.memRedirect <> io.mem.replayAll
  roq.io.dp1Req <> dispatch.io.toRoq
  dispatch.io.roqIdxs <> roq.io.roqIdxs
  io.mem.dp1Req <> dispatch.io.toLsroq
  dispatch.io.lsIdxs <> io.mem.lsIdxs
  dispatch.io.dequeueRoqIndex.valid := roq.io.commitRoqIndex.valid || io.mem.oldestStore.valid
  // store writeback must be after commit roqIdx
  dispatch.io.dequeueRoqIndex.bits := Mux(io.mem.oldestStore.valid, io.mem.oldestStore.bits, roq.io.commitRoqIndex.bits)


  intRf.io.readPorts <> dispatch.io.readIntRf ++ dispatch.io.memIntRf
  fpRf.io.readPorts <> dispatch.io.readFpRf ++ dispatch.io.memFpRf

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

  intRf.io.writePorts <> wbIntResults.map(exuOutToRfWrite)
  fpRf.io.writePorts <> wbFpResults.map(exuOutToRfWrite)

  rename.io.wbIntResults <> wbIntResults
  rename.io.wbFpResults <> wbFpResults

  roq.io.exeWbResults.take(exeWbReqs.length).zip(wbu.io.toRoq).foreach(x => x._1 := x._2)
  roq.io.exeWbResults.last := DontCare

  // regfile debug
  val debugIntReg, debugFpReg = WireInit(VecInit(Seq.fill(32)(0.U(XLEN.W))))
  ExcitingUtils.addSink(debugIntReg, "DEBUG_INT_ARCH_REG", ExcitingUtils.Debug)
  ExcitingUtils.addSink(debugFpReg, "DEBUG_FP_ARCH_REG", ExcitingUtils.Debug)
  val debugArchReg = WireInit(VecInit(debugIntReg ++ debugFpReg))
  if (!env.FPGAPlatform) {
    ExcitingUtils.addSource(debugArchReg, "difftestRegs", ExcitingUtils.Debug)
  }
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

  lazy val module = new LazyModuleImp(this) with HasXSLog with HasXSParameter {
    val io = IO(new CacheSystemTestIO)

    val backend = Module(new FakeBackend)
    val mem = Module(new Memend)

    val dcacheModule = dcache.module
    val ptwModule = ptw.module

    mem.io.backend <> backend.io.mem
    backend.io.cfInput <> io.testVec

    ptwModule.io.tlb(0) <> mem.io.ptw
    ptwModule.io.tlb(1) <> DontCare

    dcacheModule.io.lsu.load <> mem.io.loadUnitToDcacheVec
    dcacheModule.io.lsu.lsroq <> mem.io.loadMiss
    dcacheModule.io.lsu.atomics <> mem.io.atomics
    dcacheModule.io.lsu.store <> mem.io.sbufferToDcache
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



class CacheSystemTest extends FlatSpec with ChiselScalatestTester with Matchers {

  top.Parameters.set(top.Parameters.defaultParameters)

  val annos = Seq(
    VerilatorBackendAnnotation,
    LineCoverageAnnotation,
    RunFirrtlTransformAnnotation(new PrintModuleName)
  )

  //test wdata

  val r = scala.util.Random

  it should "run" in {

    implicit val p = Parameters((site, up, here) => {
      case L2CacheSystemTestKey =>
        L2CacheSystemTestParams()
    })

    test(LazyModule(new CacheSystemTestTopWrapper()).module).withAnnotations(annos){ c =>
      c.clock.step(10)
      //TODO: Test Vector Generation
      //c.io.testVec <> DontCare
    }
  }
}

case class Req(
  addr: Int,
  wdata: BigInt,
  src1Reg: Int,
  src2Reg: Int,
  destReg: Int
) {
  override def toString() : String = {
    return f"addr: $addr%x wdata: $wdata%x src1Reg: $src1Reg%d src2Reg: $src2Reg%d "
  }  
}

case class QueueEntry(
  var id: Int, // it's transaction id
  var issued: Boolean,
  req: Req
) {
  override def toString() : String = {
    return f"id: $id%d req: $req"
  }
}


class Queue(nEntries: Int, width: Int, name: String){
  val queue = new ArrayBuffer[QueueEntry]()

  def enq(req: Req, id: Int) = {
    queue += new QueueEntry(id, false, req)
  }

  def select(): Int = {
    for (i <- 0 until queue.size) {
      if (queue(i).issued == false)
        return i
    }
    return -1
  }

  def retire(tID: Int) = {
    println(f"$name retire transaction: $tId%d")
    for (i <- 0 until queue.size) {
      if (queue(i).id == tId) {
        // remove this request
        queue.remove(i)
        println(f"$name retire req: $i%d transaction: $tId%d")
        return
      }
    }
  }

  def issued(idx: Int, tId: Int) = {
    //println(f"$name issue req: $idx%d transaction: $tId%d")
    assert(queue(idx).issued == false)
    queue(idx).issued = true
    queue(idx).tID = tId
  }

  def lookUp(tID: Int): Req = {
    for(i <- 0 util 
    ){
      if(queue(i).id == tId) {
        return queue(i).req
      }
    }
    return Req(0,0,0,0,0)
  }

  var reqWaiting = Array[Boolean](width)
  def init(): Unit = {
    queue.clear()
    for(i <- 0 until width){
        reqWaiting(i) = false    
    }
  }

  def isFinished() = queue.isEmpty

}