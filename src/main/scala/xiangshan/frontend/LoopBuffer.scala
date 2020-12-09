package xiangshan.frontend

import chisel3._
import chisel3.util._
import utils._
import xiangshan._

trait HasLBConst {
  val maxBody = 64
  val minIter = 30
  val loopDetectorTableSize = 8
  val signatureSize = 64
}

class LDTPtr extends CircularQueuePtr(8) {}

object LDTPtr {
  def apply(f: Bool, v: UInt): LDTPtr = {
    val ptr = Module(new LDTPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class LoopDetectorTableEntry extends HasXSParameter with HasLBConst {
  val pc           = UInt(VAddrBits.W)
  val bodySize     = UInt(log2Up(maxBody).W)
  val signature    = UInt(signatureSize.W) // 需要吗？
  val curBodySize  = UInt(log2Up(maxBody).W)
  val iterNum      = UInt(log2Up(minIter).W)
  val firstIterBit = Bool()
}

class LoopDetector extends HasLBConst {
//  val table = Mem(loopDetectorTableSize, new LoopDetectorTableEntry)
  val table = Reg(Vec(loopDetectorTableSize, new LoopDetectorTableEntry))
  val valid = RegInit(VecInit(Seq.fill(loopDetectorTableSize)(false.B)))
  val size  = RegInit(0.U(log2Up(loopDetectorTableSize).W))

  val buffer     = Mem(maxBody, new CfCtrl)
  val bufferSize = RegInit(0.U(log2Up(maxBody).W))
  val bufferPtr  = RegInit(0.U(log2Up(maxBody).W))
}

class LDTFreeList extends HasLBConst {
  val list = RegInit(VecInit((0 until loopDetectorTableSize).map(_.U)))
  val head = LDTPtr(false.B, 0.U)
  val tail = LDTPtr(false.B, 0.U)
  val freeNum = RegInit(loopDetectorTableSize.U)
}

class LoopBufIO extends XSBundle {
  val flush = Input(Bool())
  val in    = Vec(DecodeWidth, DecoupledIO(new CfCtrl))
  val out   = Vec(DecodeWidth, DecoupledIO(new CfCtrl))
}

class LoopBuffer extends XSModule with HasLBConst with HasCircularQueuePtrHelper{
  val io = IO(new LoopBufIO())

  def isBackwardBranch(inst: UInt):Bool = {
    val isJal   = inst === BitPat("b1???????????????????_?????_1101111")
    val isCon   = inst === BitPat("b1??????_?????_?????_???_?????_1100011")
    val isC_Jal = inst === BitPat("b????????????????_001_1??????????_01")
    val isC_Con = inst === BitPat("b????????????????_11?_1??_???_?????_01")

    isJal || isCon || isC_Jal || isC_Con
  }

  val backwardBranchVec = io.in.map(i => i.fire && isBackwardBranch(i.bits.cf.instr))
  val haveBackwardBranch = backwardBranchVec.reduce(_||_)

  // FSM state define
  val s_idle :: s_fill :: s_active :: Nil = Enum(3)
  val LBstate = RegInit(s_idle)
  val loopEntry = Reg(UInt(log2Up(loopDetectorTableSize).W))

  val loopDetector = Module(new LoopDetector)
  val freeList = new LDTFreeList

  val instNum = io.in.map(_.fire).fold(0.U(log2Up(DecodeWidth).W))(_+_)

  def allocateEntry(n: UInt) = {
    when(freeList.freeNum >= n) {
      freeList.head := freeList.head + n
      freeList.freeNum := freeList.freeNum - n
      n
    }.otherwise {
      freeList.head := freeList.head + freeList.freeNum
      freeList.freeNum := 0.U
      freeList.freeNum
    }
  }

  // Update curBodySize
  for(i <- 0 until loopDetectorTableSize) {
    when(loopDetector.valid(i)) {
      val newBodySize = loopDetector.table(i).curBodySize + instNum
      loopDetector.table(i).curBodySize := newBodySize

      when(newBodySize > maxBody.U) { loopDetector.valid(i) := false.B }
    }
  }

  val missVec = WireInit(VecInit(Seq.fill(DecodeWidth)(false.B)))

  // update iterNum
  for(i <- 0 until DecodeWidth) {
    when(isBackwardBranch(io.in(i).bits.cf.instr)) {
      val searchLDTVec = loopDetector.table.map(io.in(i).bits.cf.pc === _.pc)
      val inLDT = searchLDTVec.reduce(_||_)
      val idx = OHToUInt(PriorityEncoderOH(searchLDTVec)) // TODO: Need reverse?
      when(inLDT) {
        val matchEntry = loopDetector.table(idx)
        when(matchEntry.firstIterBit || matchEntry.curBodySize === matchEntry.bodySize) {
          matchEntry.iterNum := matchEntry.iterNum + 1.U
          when(matchEntry.iterNum + 1.U === minIter.U) {
            LBstate := s_fill
            loopEntry := idx
          }
        }.otherwise {
          matchEntry.iterNum := 0.U
          matchEntry.bodySize := matchEntry.curBodySize
        }
      }.otherwise {
//        assert(loopDetector.size < loopDetectorTableSize.U)
        missVec(idx) := true.B
      }
    }
  }

  allocateEntry(PopCount(missVec))
  var enqPtr = freeList.tail
  for(i <- 0 until DecodeWidth) {
    when(missVec(i) && !loopDetector.valid(enqPtr.value)) {
      val entry = loopDetector.table(enqPtr.value)
      loopDetector.valid(enqPtr.value) := true.B
      entry.pc            := io.in(i).bits.cf.pc
      entry.bodySize      := 0.U
      entry.signature     := DontCare
      entry.curBodySize   := PopCount(((~UIntToOH(i.U)).asUInt ^ (UIntToOH(i.U) - 1.U)) & Cat((0 until DecodeWidth).map(io.in(_).fire)))
      entry.iterNum       := 1.U
      entry.firstIterBit  := true.B
      enqPtr = enqPtr + 1.U
    }
  }

  when(LBstate === s_fill) {
    // Save loop
    var enqIdx = loopDetector.bufferSize
    for(i <- 0 until DecodeWidth) {
      when(io.in(i).fire) {
        loopDetector.buffer(enqIdx) := io.in(i).bits
        enqIdx = enqIdx + 1.U
      }
    }

    when(enqIdx >= loopDetector.table(loopEntry).bodySize) {
      LBstate := s_active
    }
    loopDetector.bufferSize := enqIdx
  }

  when(LBstate === s_active) {
    (0 until DecodeWidth).foreach(io.in(_).ready := false.B)
    var deqIdx = loopDetector.bufferPtr
    for(i <- 0 until DecodeWidth) {
      io.out(i).valid := deqIdx < loopDetector.bufferSize
      io.out(i).bits := loopDetector.buffer(deqIdx)
    }
  }

  when(io.flush) {
    LBstate := s_idle
    loopDetector.valid := 0.U.asTypeOf(Vec(loopDetectorTableSize, Bool))
  }
}