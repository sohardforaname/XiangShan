package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import utils.{LookupTree, LookupTreeDefault, SignExt, XSDebug, ZeroExt}
import xiangshan._
import xiangshan.backend.ALUOpType
import utils.XSDebug

class Alu extends FunctionUnit with HasRedirectOut {

  val (src1, src2, offset, func, pc, uop) = (
    io.in.bits.src(0),
    io.in.bits.src(1),
    io.in.bits.uop.ctrl.imm,
    io.in.bits.uop.ctrl.fuOpType,
    SignExt(io.in.bits.uop.cf.pc, AddrBits),
    io.in.bits.uop
  )

  val redirectHit = uop.roqIdx.needFlush(io.redirectIn)
  val valid = io.in.valid && !redirectHit

  val isAdderSub = (func =/= ALUOpType.add) && (func =/= ALUOpType.addw)
  val adderRes = (src1 +& (src2 ^ Fill(XLEN, isAdderSub))) + isAdderSub
  val xorRes = src1 ^ src2
  val sltu = !adderRes(XLEN)
  val slt = xorRes(XLEN-1) ^ sltu

  val shsrc1 = LookupTreeDefault(func, src1, List(
    ALUOpType.srlw -> ZeroExt(src1(31,0), 64),
    ALUOpType.sraw -> SignExt(src1(31,0), 64)
  ))
  val shamt = Mux(ALUOpType.isWordOp(func), src2(4, 0), src2(5, 0))
  val res = LookupTreeDefault(func(3, 0), adderRes, List(
    ALUOpType.sll  -> ((shsrc1  << shamt)(XLEN-1, 0)),
    ALUOpType.slt  -> ZeroExt(slt, XLEN),
    ALUOpType.sltu -> ZeroExt(sltu, XLEN),
    ALUOpType.xor  -> xorRes,
    ALUOpType.srl  -> (shsrc1  >> shamt),
    ALUOpType.or   -> (src1  |  src2),
    ALUOpType.and  -> (src1  &  src2),
    ALUOpType.sra  -> ((shsrc1.asSInt >> shamt).asUInt)
  ))

  val branchOpTable = List(
    ALUOpType.getBranchType(ALUOpType.beq)  -> !xorRes.orR,
    ALUOpType.getBranchType(ALUOpType.blt)  -> slt,
    ALUOpType.getBranchType(ALUOpType.bltu) -> sltu
  )

  val isBranch = uop.cf.brUpdate.pd.isBr// ALUOpType.isBranch(func)
  val isRVC = uop.cf.brUpdate.pd.isRVC//(io.in.bits.cf.instr(1,0) =/= "b11".U)
  val taken = LookupTree(ALUOpType.getBranchType(func), branchOpTable) ^ ALUOpType.isBranchInvert(func)
  val target = Mux(isBranch, pc + offset, adderRes)(VAddrBits-1,0)
  val snpc = Mux(isRVC, pc + 2.U, pc + 4.U)

  redirectOutValid := io.out.valid && isBranch
  redirectOut.pc := uop.cf.pc
  redirectOut.target := Mux(!taken && isBranch, snpc, target)
  redirectOut.brTag := uop.brTag
  redirectOut.level := RedirectLevel.flushAfter
  redirectOut.interrupt := DontCare
  redirectOut.roqIdx := uop.roqIdx

  //deal with sfb
  val is_sfb_br = uop.is_sfb_br
  val is_sfb_shadow = uop.is_sfb_shadow
  val lrs1_is_lrd = uop.ctrl.lrs1_is_lrd && is_sfb_shadow
  val pred_yes = is_sfb_shadow && uop.predData
  val is_mov = (func === ALUOpType.mov) && is_sfb_shadow

  val aluRes = Mux(is_sfb_br, taken.asUInt,
                      Mux(pred_yes,
                          Mux(lrs1_is_lrd, src1, src2),
                          Mux(is_mov, src2 ,res))
                  )

  val aluResOut = Mux(ALUOpType.isWordOp(func), SignExt(aluRes(31,0), 64), aluRes)

  XSDebug(io.out.valid,"[ALU-sfb]:pc:%x isbr:%d isShadow:%d pred_yes:%d is_mov:%d lrs1_is_ld:%d\n",
                uop.cf.pc,is_sfb_br,is_sfb_shadow,pred_yes,is_mov,lrs1_is_lrd)


  brUpdate := uop.cf.brUpdate
  // override brUpdate
  brUpdate.pc := uop.cf.pc
  brUpdate.target := Mux(!taken && isBranch, snpc, target)
  brUpdate.brTarget := target
  brUpdate.taken := isBranch && taken
  brUpdate.brTag := uop.brTag

  io.in.ready := io.out.ready
  io.out.valid := valid
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := aluResOut
}
