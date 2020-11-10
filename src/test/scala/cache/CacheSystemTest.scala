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

  // regfile debug
  val debugIntReg, debugFpReg = WireInit(VecInit(Seq.fill(32)(0.U(XLEN.W))))
  BoringUtils.addSink(debugIntReg, "DEBUG_INT_ARCH_REG")
  BoringUtils.addSink(debugFpReg, "DEBUG_FP_ARCH_REG")
  val debugArchReg = WireInit(VecInit(debugIntReg ++ debugFpReg))
  if (!env.FPGAPlatform) {
    BoringUtils.addSource(debugArchReg, "difftestRegs")
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


    //default
    val difftest = WireInit(0.U.asTypeOf(new DiffTestIO))
    BoringUtils.addSink(difftest.commit, "difftestCommit")
    BoringUtils.addSink(difftest.thisPC, "difftestThisPC")
    BoringUtils.addSink(difftest.thisINST, "difftestThisINST")
    BoringUtils.addSink(difftest.skip, "difftestSkip")
    BoringUtils.addSink(difftest.isRVC, "difftestIsRVC")
    BoringUtils.addSink(difftest.wen, "difftestWen")
    BoringUtils.addSink(difftest.wdata, "difftestWdata")
    BoringUtils.addSink(difftest.wdst, "difftestWdst")
    BoringUtils.addSink(difftest.wpc, "difftestWpc")
    BoringUtils.addSink(difftest.intrNO, "difftestIntrNO")
    BoringUtils.addSink(difftest.cause, "difftestCause")
    BoringUtils.addSink(difftest.r, "difftestRegs")
    BoringUtils.addSink(difftest.priviledgeMode, "difftestMode")
    BoringUtils.addSink(difftest.mstatus, "difftestMstatus")
    BoringUtils.addSink(difftest.sstatus, "difftestSstatus")
    BoringUtils.addSink(difftest.mepc, "difftestMepc")
    BoringUtils.addSink(difftest.sepc, "difftestSepc")
    BoringUtils.addSink(difftest.mtval, "difftestMtval")
    BoringUtils.addSink(difftest.stval, "difftestStval")
    BoringUtils.addSink(difftest.mtvec, "difftestMtvec")
    BoringUtils.addSink(difftest.stvec, "difftestStvec")
    BoringUtils.addSink(difftest.mcause, "difftestMcause")
    BoringUtils.addSink(difftest.scause, "difftestScause")
    BoringUtils.addSink(difftest.satp, "difftestSatp")
    BoringUtils.addSink(difftest.mip, "difftestMip")
    BoringUtils.addSink(difftest.mie, "difftestMie")
    BoringUtils.addSink(difftest.mscratch, "difftestMscratch")
    BoringUtils.addSink(difftest.sscratch, "difftestSscratch")
    BoringUtils.addSink(difftest.mideleg, "difftestMideleg")
    BoringUtils.addSink(difftest.medeleg, "difftestMedeleg")
    BoringUtils.addSink(difftest.scFailed, "difftestScFailed")

    val trap = WireInit(0.U.asTypeOf(new TrapIO))
    ExcitingUtils.addSink(trap.valid, "trapValid")
    ExcitingUtils.addSink(trap.code, "trapCode")
    ExcitingUtils.addSink(trap.pc, "trapPC")
    ExcitingUtils.addSink(trap.cycleCnt, "trapCycleCnt")
    ExcitingUtils.addSink(trap.instrCnt, "trapInstrCnt")

    val defaultWire = WireInit(false.B)
    BoringUtils.addSink(defaultWire, "FenceI")
    ExcitingUtils.addSink(defaultWire, "isWFI")
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

  top.Parameters.set(top.Parameters.uniTestParame)

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

     test(LazyModule(new CacheSystemTestTopWrapper()).module)
      .withAnnotations(annos){ c =>
        //c.io.testVec
        c.clock.step(10)
        //TODO: Test Vector Generation
        def gen_st_vec() = {
          for(i <- 0 until nRegs){
          }
          
        }

        def gen_ld_vec() = {

        }


      }
  }
}

case class StReq(
  addr: Long,
  wdata: BigInt,
  stype: Int,
  srcReg: Int,
  offset: Int
) {
  override def toString() : String = {
    return f"addr: $addr%x wdata: $wdata%x sype: $stype%d srcReg: $srcReg%d offset: $offset%x"
  }  
}

case class QueueEntry(
  var id: Int, // it's transaction id
  req: StReq
) {
  override def toString() : String = {
    return f"id: $id%d req: $req"
  }
}


class CfStQueue(nEntries: Int, width: Int, name: String){
  val queue = new ArrayBuffer[QueueEntry]()
  val IdPool = new IdPool(nEntries)

  def enq(req: StReq) = {
    queue += new QueueEntry(-1, req)
  }

  def select(): Int = {
    for (i <- 0 until queue.size) {
      if (queue(i).id == -1)
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

  def issue(idx: Int, tId: Int) = {
    println(f"$name issue req: $idx%d transaction: $tId%d")
    assert(queue(idx).id == -1)
    queue(idx).id = tId
  }

  def lookUp(tID: Int): Array[StReq] = {
    for(i <- 0 util nEntries){
      if(queue(i).id == tId) {
        return queue(i).req
      }
    }
    return StReq(0,0,0,0,0)
  }

  var reqWaiting = Array[Boolean](width)
  def init(): Unit = {
    idPool.init()
    queue.clear()
    reqWaiting = false    
  }

  def isFinished() = queue.isEmpty

  def sendOneSt(reqvec: CacheSystemTestIO): Int ={
    val inputVec = reqvec.testVec

    //if last req sent and allow to in
    //reset the flag and valid
    for(i <- 0 until width){
      if(reqWaiting && input(i).ready.peek().litToBoolean){
        reqWaiting(i) = false
        input(i).valid.poke(false.B)
      }
    }

    val reqIdx = select()
    if(reqWaiting || reqIdx == -1){
      println(s"req can not be sent!")
      return -1
    }

    val tID = idPool.allocate()
    if (tID == -1) {
      println(s"no trasaction id availabe")
      return -1
    }

    val inputGateNum = tID % width
    val input = inputVec(inputGateNum)

    reqWaiting = true

    issue(reqIdx,tID)

    val r = queue(reqIdx).req
    input.valid.poke(true.B)
    input.bits.cf.pc.poke(tID.U)
    input.bits.ctrl.src1Type.poke(SrcType.reg)
    input.bits.ctrl.src2Type.poke(SrcType.reg)
    input.bits.ctrl.lsrc1.poke(r.srcReg.U)
    input.bits.ctrl.lsrc2.poke(r.addr.U)
    input.bits.ctrl.fuType.poke(FuType.stu)
    input.bits.ctrl.fuOpType.poke(LSUOpType.sd)
    input.bits.ctrl.imm.poke(r.offset.U)

    return tID 
  }


}