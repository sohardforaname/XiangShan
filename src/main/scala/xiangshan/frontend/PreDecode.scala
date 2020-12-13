package xiangshan.frontend

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.backend.decode.isa.predecode.PreDecodeInst

trait HasPdconst{ this: XSModule =>
  def isRVC(inst: UInt) = (inst(1,0) =/= 3.U)
  def isLink(reg:UInt) = reg === 1.U || reg === 5.U
  def brInfo(instr: UInt) = {
    val brType::Nil = ListLookup(instr, List(BrType.notBr), PreDecodeInst.brTable)
    val rd = Mux(isRVC(instr), instr(12), instr(11,7))
    val rs = Mux(isRVC(instr), Mux(brType === BrType.jal, 0.U, instr(11, 7)), instr(19, 15))
    val isCall = (brType === BrType.jal && !isRVC(instr) || brType === BrType.jalr) && isLink(rd) // Only for RV64
    val isRet = brType === BrType.jalr && isLink(rs) && !isCall
    List(brType, isCall, isRet)
  }
  def getRd(instr: UInt) = instr(11,7) //TODO: add RVC
  def getRs1(instr: UInt) = instr(19,15)

}

object BrType {
  def notBr   = "b00".U
  def branch  = "b01".U
  def jal     = "b10".U
  def jalr    = "b11".U
  def apply() = UInt(2.W)
}

object SfbConst {
  def T = true.B
  def F = false.B
}

object ExcType {  //TODO:add exctype
  def notExc = "b000".U
  def apply() = UInt(3.W)
}

object MaskLower
{
  def apply(in: UInt) = {
    val n = in.getWidth
    (0 until n).map(i => in >> i.U).reduce(_|_)
  }
}

object MaskUpper
{
  def apply(in: UInt) = {
    val n = in.getWidth
    (0 until n).map(i => (in << i.U)(n-1,0)).reduce(_|_)
  }
}

class PreDecodeInfo extends XSBundle {  // 8 bit
  val isRVC   = Bool()
  val brType  = UInt(2.W)
  val isCall  = Bool()
  val isRet   = Bool()
  val excType = UInt(3.W)
  def isBr = brType === BrType.branch
  def isJal = brType === BrType.jal
  def isJalr = brType === BrType.jalr
  def notCFI = brType === BrType.notBr
}

class PreDecodeResp extends XSBundle with HasIFUConst {
  val instrs = Vec(PredictWidth, UInt(32.W))
  val pc = Vec(PredictWidth, UInt(VAddrBits.W))
  val mask = UInt(PredictWidth.W)
  // one for the first bank
  val lastHalf = UInt(nBanksInPacket.W)
  val pd = Vec(PredictWidth, (new PreDecodeInfo))
  val sfbVec = Vec(PredictWidth, Bool())
  val shadowableVec = Vec(PredictWidth, Bool())
  val rangeMask =  Vec(PredictWidth, UInt(PredictWidth.W))
}

class PreDecode extends XSModule with HasPdconst with HasIFUConst {
  val io = IO(new Bundle() {
    val in = Input(new ICacheResp)
    val prev = Flipped(ValidIO(UInt(16.W)))
    val out = Output(new PreDecodeResp)
  })

  val data = io.in.data
  val mask = io.in.mask
  
  val validCount = PopCount(mask)
  val bankAlignedPC = bankAligned(io.in.pc)
  val bankOffset = offsetInBank(io.in.pc)
  val isAligned = bankOffset === 0.U

  val firstValidIdx = bankOffset // io.prev.valid should only occur with firstValidIdx = 0
  XSError(firstValidIdx =/= 0.U && io.prev.valid, p"pc:${io.in.pc}, mask:${io.in.mask}, prevhalfInst valid occurs on unaligned fetch packet\n")
  // val lastHalfInstrIdx = Mux(isInLastBank(pc), (bankWidth-1).U, (bankWidth*2-1).U)
  // in case loop buffer gives a packet ending at an unaligned position
  val lastHalfInstrIdx = PriorityMux(Reverse(mask), (PredictWidth-1 to 0 by -1).map(i => i.U))

  val insts = Wire(Vec(PredictWidth, UInt(32.W)))
  val instsMask = Wire(Vec(PredictWidth, Bool()))
  val instsEndMask = Wire(Vec(PredictWidth, Bool()))
  val instsRVC = Wire(Vec(PredictWidth,Bool()))
  val instsPC = Wire(Vec(PredictWidth, UInt(VAddrBits.W)))

  val rawInsts = VecInit((0 until PredictWidth).map(i => if (i == PredictWidth-1) Cat(0.U(16.W), data(i*16+15, i*16))
                                                         else data(i*16+31, i*16)))
  // val nextHalf = Wire(UInt(16.W))

  val lastHalf = Wire(Vec(nBanksInPacket, UInt(1.W)))

  for (i <- 0 until PredictWidth) {
    val inst = WireInit(rawInsts(i))
    val validStart = Wire(Bool()) // is the beginning of a valid inst
    val validEnd = Wire(Bool())  // is the end of a valid inst
    val pc = bankAlignedPC + (i << 1).U - Mux(io.prev.valid && (i.U === firstValidIdx), 2.U, 0.U)

    val isFirstInPacket = i.U === firstValidIdx
    val isLastInPacket = i.U === lastHalfInstrIdx
    val currentRVC = isRVC(insts(i))

    val lastIsValidEnd = if (i == 0) { !io.prev.valid } else { instsEndMask(i-1) || isFirstInPacket }
    
    inst := Mux(io.prev.valid && i.U === 0.U, Cat(rawInsts(i)(15,0), io.prev.bits), rawInsts(i))

    validStart := lastIsValidEnd && !(isLastInPacket && !currentRVC)
    validEnd := validStart && currentRVC || !validStart && !(isLastInPacket && !currentRVC)

    val currentLastHalf = lastIsValidEnd && (isLastInPacket && !currentRVC)

    insts(i) := inst
    instsRVC(i) := isRVC(inst)
    instsMask(i) := (if (i == 0) Mux(io.prev.valid, validEnd, validStart) else validStart)
    instsEndMask(i) := validEnd
    instsPC(i) := pc

    val brType::isCall::isRet::Nil = brInfo(inst)
    io.out.pd(i).isRVC := instsRVC(i)
    io.out.pd(i).brType := brType
    io.out.pd(i).isCall := isCall
    io.out.pd(i).isRet := isRet
    io.out.pd(i).excType := ExcType.notExc
    io.out.instrs(i) := insts(i)
    io.out.pc(i) := instsPC(i)

    //for sfb

    val branchOffset = Cat(inst(7), inst(30,25), inst(11,8), 0.U(1.W))
    val offsetIdx = branchOffset(log2Ceil(PredictWidth),1)
    val isBr = (brType === BrType.branch)
    val isInBound = Mux(isBr, (offsetIdx < (PredictWidth - i).U),false.B) //TODO: FIX ME!!!
    val shadowable::has_rs2::Nil = ListLookup(inst,List(SfbConst.F, SfbConst.F),PreDecodeInst.sfbTable)

    val sfbOH = Wire(UInt(PredictWidth.W))
    val tgtOH = Wire(UInt(PredictWidth.W))
    sfbOH  := UIntToOH(i.U)
    tgtOH  := UIntToOH((i.U + offsetIdx)(log2Ceil(PredictWidth)-1,0)) 

    io.out.rangeMask(i) := ~MaskLower(sfbOH) & ~MaskUpper(tgtOH)
    io.out.sfbVec(i) := isBr && !inst(31) && branchOffset =/= 0.U && isInBound && instsMask(i)  //TODO: better parameterizing
    io.out.shadowableVec(i) := instsMask(i) && shadowable && (
          !has_rs2 ||
          getRs1(inst) === getRd(inst) ||
          (inst === PreDecodeInst.ADD) && getRs1(inst) === 0.U
    )
    
    if (i == bankWidth-1)    { lastHalf(0) := currentLastHalf }
    if (i == PredictWidth-1) { lastHalf(1) := currentLastHalf }
  }
  io.out.mask := instsMask.asUInt & mask
  io.out.lastHalf := lastHalf.asUInt

  for (i <- 0 until PredictWidth) {
    XSDebug(true.B,
      p"instr ${Hexadecimal(io.out.instrs(i))}, " +
      p"mask ${Binary(instsMask(i))}, " +
      p"endMask ${Binary(instsEndMask(i))}, " +
      p"pc ${Hexadecimal(io.out.pc(i))}, " +
      p"isRVC ${Binary(io.out.pd(i).isRVC)}, " +
      p"brType ${Binary(io.out.pd(i).brType)}, " +
      p"isRet ${Binary(io.out.pd(i).isRet)}, " +
      p"isCall ${Binary(io.out.pd(i).isCall)}\n"
    )
  }

  for (i <- 0 until PredictWidth) {
    XSDebug("[SFB](%d) sfb_range_mask:%x   shadowableVec:%d   sfbVec:%d\n",i.U,io.out.rangeMask(i),io.out.shadowableVec(i),io.out.sfbVec(i))
  }

}

     