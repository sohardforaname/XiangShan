package cache


import scala.collection.mutable.ArrayBuffer
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
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import top.{DiffTestIO, TrapIO}
import utils.{DebugIdentityNode, HoldUnless, XSDebug}
import xiangshan.HasXSLog
import xiangshan._
import xiangshan.{XSCoreParameters}
import xiangshan.backend._
import xiangshan.backend.exu._
import xiangshan.backend.dispatch._
import xiangshan.backend.roq._
import xiangshan.backend.regfile._
import xiangshan.backend.issue._
import xiangshan.backend.rename._
import xiangshan.mem._
import xiangshan.cache.{DCache, DCacheLineReq, DCacheWordReq, MemoryOpConstants, PTW}
import xiangshan.backend.fu.FunctionUnit._
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

class CommitBundle extends XSBundle {
  val roqCommits = Vec(CommitWidth, Valid(new RoqCommit))
  val commitWdata = Output(Vec(CommitWidth, UInt(XLEN.W)))
}

class CacheSystemTestIO extends XSBundle {
  val testVec = Vec(RenameWidth, Flipped(DecoupledIO(new CfCtrl)))
  val retiredVec = new CommitBundle
}

class FakeBackend extends Module with HasXSParameter {
  val io = IO(new Bundle {
    val cfInput = Vec(RenameWidth, Flipped(DecoupledIO(new CfCtrl)))
    val roqOutput = new CommitBundle
    val mem = Flipped(new MemToBackendIO)
  })

  val aluExeUnits =Array.tabulate(exuParameters.AluCnt)(_ => Module(new AluExeUnit))
  val jmpExeUnit = Module(new JumpExeUnit)
  val mduExeUnits = Array.tabulate(exuParameters.MduCnt)(_ => Module(new MulDivExeUnit))
  val fmacExeUnits = Array.tabulate(exuParameters.FmacCnt)(_ => Module(new FmacExeUnit))
  val fmiscExeUnits = Array.tabulate(exuParameters.FmiscCnt)(_ => Module(new FmiscExeUnit))
  val exeUnits = jmpExeUnit +: (aluExeUnits ++ mduExeUnits ++ fmacExeUnits ++ fmiscExeUnits)
  exeUnits.foreach(_.io.csrOnly := DontCare)
  exeUnits.foreach(_.io.mcommit := DontCare)

  fmacExeUnits.foreach(_.frm := jmpExeUnit.frm)
  fmiscExeUnits.foreach(_.frm := jmpExeUnit.frm)

  val ldExeUnitCfg = ExuConfig("LoadExu", Seq(lduCfg), wbIntPriority = 0, wbFpPriority = 0)
  val stExeUnitCfg = ExuConfig("StoreExu", Seq(stuCfg, mouCfg), wbIntPriority = Int.MaxValue, wbFpPriority = Int.MaxValue)

  val rename = Module(new Rename)
  val dispatch = Module(new Dispatch(
    jmpExeUnit.config, aluExeUnits(0).config, mduExeUnits(0).config,
    fmacExeUnits(0).config, fmiscExeUnits(0).config,
    ldExeUnitCfg, stExeUnitCfg
  ))
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
    Seq.fill(exuParameters.LduCnt)(ldExeUnitCfg) ++
    Seq.fill(exuParameters.StuCnt)(stExeUnitCfg)

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

    val feedback = (cfg == ldExeUnitCfg) || (cfg == stExeUnitCfg)
    
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
      case `ldExeUnitCfg` =>
      case `stExeUnitCfg` =>
      case otherCfg =>
        exeUnits(i).io.in <> rs.io.deq
        exeUnits(i).io.redirect <> redirect
        rs.io.tlbFeedback := DontCare
    }

    rs.suggestName(s"rs_${cfg.name}")

    rs
  })



  for(rs <- reservedStations){
    rs.io.broadcastedUops <> reservedStations.
      filter(x => x.exuCfg.hasCertainLatency && needData(rs.exuCfg, x.exuCfg)).
      map(_.io.selectedUop)
  }


  io.mem.commits <> roq.io.commits
  io.mem.roqDeqPtr := roq.io.roqDeqPtr

  io.mem.ldin <> reservedStations.filter(_.exuCfg == ldExeUnitCfg).map(_.io.deq)
  io.mem.stin <> reservedStations.filter(_.exuCfg == stExeUnitCfg).map(_.io.deq)
  jmpExeUnit.io.csrOnly.exception.valid := roq.io.redirect.valid && roq.io.redirect.bits.isException
  jmpExeUnit.io.csrOnly.exception.bits := roq.io.exception
  jmpExeUnit.fflags := roq.io.fflags
  jmpExeUnit.dirty_fs := roq.io.dirty_fs
  jmpExeUnit.io.csrOnly.externalInterrupt := DontCare
  jmpExeUnit.io.csrOnly.memExceptionVAddr := io.mem.exceptionAddr.vaddr
  jmpExeUnit.fenceToSbuffer <> io.mem.fenceToSbuffer
  io.mem.sfence <> jmpExeUnit.sfence
  io.mem.csr <> jmpExeUnit.tlbCsrIO

  io.mem.exceptionAddr.lsIdx.lsroqIdx := roq.io.exception.lsroqIdx
  io.mem.exceptionAddr.lsIdx.lqIdx := roq.io.exception.lqIdx
  io.mem.exceptionAddr.lsIdx.sqIdx := roq.io.exception.sqIdx
  io.mem.exceptionAddr.isStore := CommitType.lsInstIsStore(roq.io.exception.ctrl.commitType)

  io.mem.tlbFeedback <> reservedStations.filter(
    x => x.exuCfg == ldExeUnitCfg || x.exuCfg == stExeUnitCfg
  ).map(_.io.tlbFeedback)

  

  rename.io.redirect <> redirect
  rename.io.roqCommits <> roq.io.commits
  rename.io.in <> io.cfInput
  rename.io.intRfReadAddr <> dispatch.io.readIntRf.map(_.addr) ++ dispatch.io.memIntRf.map(_.addr)
  rename.io.intPregRdy <> dispatch.io.intPregRdy ++ dispatch.io.intMemRegRdy
  rename.io.fpRfReadAddr <> dispatch.io.readFpRf.map(_.addr) ++ dispatch.io.memFpRf.map(_.addr)
  rename.io.fpPregRdy <> dispatch.io.fpPregRdy ++ dispatch.io.fpMemRegRdy
  rename.io.replayPregReq <> dispatch.io.replayPregReq
  dispatch.io.redirect <> redirect
  dispatch.io.fromRename <> rename.io.out

  roq.io.memRedirect <> io.mem.replayAll
  roq.io.brqRedirect <> DontCare
  roq.io.dp1Req <> dispatch.io.toRoq
  roq.io.intrBitSet := jmpExeUnit.io.csrOnly.interrupt
  roq.io.trapTarget := jmpExeUnit.io.csrOnly.trapTarget
  dispatch.io.roqIdxs <> roq.io.roqIdxs
  io.mem.dp1Req <> dispatch.io.toLsroq
  dispatch.io.lsIdxs <> io.mem.lsIdxs
  dispatch.io.dequeueRoqIndex.valid := roq.io.commitRoqIndex.valid || io.mem.oldestStore.valid
  // store writeback must be after commit roqIdx
  dispatch.io.dequeueRoqIndex.bits := Mux(io.mem.oldestStore.valid, io.mem.oldestStore.bits, roq.io.commitRoqIndex.bits)

  // ExcitingUtils.addSink(wdata, "cacheSystemWdata", ExcitingUtils.Debug)
  io.roqOutput.roqCommits := roq.io.commits
  io.roqOutput.commitWdata := roq.io.wdata

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



class CacheSystemTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  top.Parameters.set(top.Parameters.defaultParameters)

  val annos = Seq(
    VerilatorBackendAnnotation,
    LineCoverageAnnotation,
    RunFirrtlTransformAnnotation(new PrintModuleName)
  )

  //test wdata

  val r = scala.util.Random
  val testEntries = 60
  val testWidth = 6
  val regNum = 32

  it should "run" in {

    implicit val p = Parameters((site, up, here) => {
      case L2CacheSystemTestKey =>
        L2CacheSystemTestParams()
    })

    test(LazyModule(new CacheSystemTestTopWrapper()).module).withAnnotations(annos){ c =>
      c.clock.step(10)
        var fake_regfile =  Array.tabulate(regNum)(_ => 0)
        var golden_trace = Array.tabulate(testEntries)(_ => (0,0))

        // addr data tempReg
        // the idx of trace vector is the trasaction ID
        c.reset.poke(true.B)
        c.clock.step(10)
        c.reset.poke(false.B)

        val sq = new CSStoreQueue(testEntries,testWidth)
        val lq = new CSLoadQueue(testEntries,testWidth)

        var sqPtr = 0
        var lqPtr = 0

        def gen_array(){
            for(i <- 0 until regNum){
                val addr = r.nextInt() & 0x2fffffff
                fake_regfile(i) = addr
                println(f"fake_rf_$i%d: $addr%x")
            }
        }

        def sendOneLi(ldest:Int, addr:Int, cf: CfCtrl){
          cf.ctrl.src1Type.poke(SrcType.reg)
          cf.ctrl.src2Type.poke(SrcType.imm)
          cf.ctrl.lsrc1.poke(0.U)
          cf.ctrl.ldest.poke(ldest.U)
          cf.ctrl.fuType.poke(FuType.alu)
          cf.ctrl.fuOpType.poke(ALUOpType.add)
          cf.ctrl.rfWen.poke(true.B)
          cf.ctrl.imm.poke(addr.U)
        }

        def set_lreg_file(){
          val inputVec = c.io.testVec
          var reqWaiting = false
          var literation = 32
          var entryPointer = 0

          while(literation > 0){
            var issueNum = 0
            for(i <- 0 until testWidth){
              if(inputVec(i).ready.peek().litToBoolean && entryPointer < 32){
                inputVec(i).valid.poke(true.B)
                println(f"entry Pointer: $entryPointer%d, index: $i%d")
                sendOneLi(entryPointer,fake_regfile(entryPointer),inputVec(i).bits)
                issueNum = issueNum+1
                entryPointer= entryPointer+1
              }
            }
            literation = literation - issueNum
            c.clock.step(1)
          }    

          //wait for 100 cycles to finish li instruction
          c.clock.step(50)  
        }

        def tick(){
          for(i <- 0 until testWidth){
            val rand = r.nextInt(10)
            val isSt = rand % 2 == 0
            if((!isSt && sqPtr > lqPtr) || sq.isFinished && !lq.isFinished){
              lq.sendOneReq(c.io)
              lqPtr=lqPtr+1
            }
            else{
              sq.sendOneReq(c.io)
              sqPtr=sqPtr+1
            }           
          }       
           sq.handleResp(port = c.io)
           lq.handleResp(port = c.io, trace = golden_trace)
        }

        /* -----------------------
         * 
         * Test Cache System
         *
         * -----------------------
         */

        val inputVec = c.io.testVec
        val outputVec = c.io.retiredVec

        sq.init()
        lq.init()


        gen_array()

        for(i <- 0 until testEntries){
          val dataReg  = r.nextInt(regNum)
          val addrReg  = r.nextInt(regNum)
          val addr = fake_regfile(addrReg)
          val wdata = fake_regfile(dataReg)
          golden_trace(i) = (addr,wdata)
          println(f"GT: id: $i%d adddr: $addr%x $wdata%x dataR: $dataReg%d addrR: $addrReg%d")
          val stReq = CSReq(addr=addr,wdata=wdata,src1Reg=addrReg,src2Reg=dataReg,destReg=0)
          val ldReq = CSReq(addr=addr,wdata=wdata,src1Reg=addrReg,src2Reg=0,destReg=dataReg)   //write to data source so that regfile doesn't change
          sq.enq(stReq,i)
          lq.enq(ldReq,i)
        }

        set_lreg_file()
        
        //while(!lq.isFinished){
        for(i <- 0 until 50){
          tick()
          c.clock.step(1)
        }

    }
  }
}

case class CSReq(
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


case class CSQueueEntry(
  var id: Int, // it's transaction id
  var issued: Boolean,
  req: CSReq
) {
  override def toString() : String = {
    return f"id: $id%d req: $req"
  }
}


class CSQueue(nEntries: Int, width: Int, name: String){
  val queue = new ArrayBuffer[CSQueueEntry]()
  val idPool = new IdPool(nEntries, name + "IdPool")

  def enq(req: CSReq, id: Int) = {
    queue += new CSQueueEntry(id, false, req)
  }

  def select(): Int = {
    for (i <- 0 until queue.size) {
      if (queue(i).issued == false)
        return i
    }
    return -1
  }

  def retire(tID: Int) = {
    println(f"$name retire transaction: $tID%d")
    for (i <- 0 until queue.size) {
      if (queue(i).id == tID) {
        // remove this request
        queue.remove(i)
        println(f"$name retire req: $i%d transaction: $tID%d")
      }
    }
  }

  def push(idx: Int, tID: Int) = {
    //println(f"$name issue req: $idx%d transaction: $tID%d")
    assert(queue(idx).issued == false)
    queue(idx).issued = true
    queue(idx).id = tID
  }

  def lookUp(tID: Int): CSReq = {
    for(i <- 0 until  queue.size){
      if(queue(i).id == tID) {
        return queue(i).req
      }
    }
    return CSReq(0,0,0,0,0)
  }

  var reqWaiting =  Array.tabulate(nEntries)(_ => false)
  def init(): Unit = {
    queue.clear()
    for(i <- 0 until width){
        reqWaiting(i) = false    
    }
  }

  def isFinished() = queue.isEmpty

}


class CSStoreQueue (nEntries: Int, width: Int) extends CSQueue(nEntries, width, "CSStoreQueue"){
  def sendOneReq(reqvec: CacheSystemTestIO): Int ={
    val inputVec = reqvec.testVec

    //if last req sent and allow to in
    //reset the flag and valid
    for(i <- 0 until width){
      if(reqWaiting(i) && inputVec(i).ready.peek().litToBoolean){
        reqWaiting(i) = false
        inputVec(i).valid.poke(false.B)
      }
    }

    val reqIdx = select()
    for(i <- 0 until width){
      if(reqWaiting(i) || reqIdx == -1){
      println(s"req can not be sent!")
      return -1
      }
    }

    //no free for trasaction ID
    val tID = idPool.allocate()
    if (tID == -1) {
      println(s"no trasaction id availabe")
      return -1
    }

    val inputGateNum = tID % width
    val input = inputVec(inputGateNum)

    reqWaiting(inputGateNum) = true    

    push(reqIdx,tID)

    val r = queue(reqIdx).req
    input.valid.poke(true.B)
    input.bits.cf.pc.poke(tID.U)
    input.bits.ctrl.src1Type.poke(SrcType.reg)
    input.bits.ctrl.src2Type.poke(SrcType.reg)
    input.bits.ctrl.lsrc1.poke(r.src1Reg.U)
    input.bits.ctrl.lsrc2.poke(r.src2Reg.U)
    input.bits.ctrl.fuType.poke(FuType.stu)
    input.bits.ctrl.fuOpType.poke(LSUOpType.sw)
    input.bits.ctrl.imm.poke(0.U)

    return tID 
  }

  def handleResp(port: CacheSystemTestIO) = {
    val commit = port.retiredVec.roqCommits
    // always ready
    
    for(i <- 0 until CommitWidth){
      if (commit(i).valid.peek().litToBoolean) {
        val id = commit(i).bits.uop.cf.pc.peek().litValue.longValue.toInt
        println(f"id $id%d retired in storequeue")
        //Don't free
        //idPool.free(id)
        retire(id)
      }
    }
  }

}

class CSLoadQueue (nEntries: Int, width: Int) extends CSQueue(nEntries, width, "CSLoadQueue"){
  def sendOneReq(reqvec: CacheSystemTestIO): Int ={
    val inputVec = reqvec.testVec

    //if last req sent and allow to in
    //reset the flag and valid
    for(i <- 0 until width){
      if(reqWaiting(i) && inputVec(i).ready.peek().litToBoolean){
        reqWaiting(i) = false
        inputVec(i).valid.poke(false.B)
      }
    }

    val reqIdx = select()
    for(i <- 0 until width){
      if(reqWaiting(i) || reqIdx == -1){
      println(s"req can not be sent!")
      return -1
      }
    }

    //no free for trasaction ID
    val tID = idPool.allocate()
    if (tID == -1) {
      println(s"no trasaction id availabe")
      return -1
    }

    val inputGateNum = tID % width
    val input = inputVec(inputGateNum)

    reqWaiting(inputGateNum) = true    

    push(reqIdx,tID)

    val r = queue(reqIdx).req
    input.valid.poke(true.B)
    input.bits.cf.pc.poke(tID.U)
    input.bits.ctrl.src1Type.poke(SrcType.reg)
    input.bits.ctrl.src2Type.poke( SrcType.imm)
    input.bits.ctrl.lsrc1.poke(r.src1Reg.U)
    input.bits.ctrl.ldest.poke(r.destReg.U)
    input.bits.ctrl.fuType.poke(FuType.ldu)
    input.bits.ctrl.fuOpType.poke(LSUOpType.lw)
    input.bits.ctrl.imm.poke(0.U)

    return tID 
  }

  def handleResp(port: CacheSystemTestIO, trace: Array[(Int,Int)]) = {
    val commit = port.retiredVec.roqCommits
    val wdatas  = port.retiredVec.commitWdata
    // always ready
    
    for(i <- 0 until CommitWidth){
        if (commit(i).valid.peek().litToBoolean) {
            val id = commit(i).bits.uop.cf.pc.peek().litValue.longValue.toInt
            val wdata = wdatas(i).peek().litValue.longValue.toInt
            println(f"id $id%d retired in loadqueue, wdata :$wdata%x")
            //Don't free
            //idPool.free(id)
            retire(id)
            assert(commit(i).bits.uop.cf.pc == trace(id)._1 && wdata == trace(i)._2,"Error: wrong addr  or with wrong data")
        }
    }
  }
}