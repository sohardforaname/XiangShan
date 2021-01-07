//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package xiangshan.backend.decode

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.{RVCDecoder, ExpandedInstruction}
import freechips.rocketchip.rocket.{CSR,Causes}
import freechips.rocketchip.util.{uintToBitPat,UIntIsOneOf}

import xiangshan._
import utils._
import xiangshan.backend._
import xiangshan.backend.decode.Instructions._
import xiangshan.backend.fu.fpu.FPUOpType
import freechips.rocketchip.tile.RocketTile

/**
 * Abstract trait giving defaults and other relevant values to different Decode constants/
 */
abstract trait DecodeConstants {
  def X = BitPat("b?")
  def N = BitPat("b0")
  def Y = BitPat("b1")

  def decodeDefault: List[BitPat] = // illegal instruction
    //   src1Type     src2Type     src3Type     fuType      fuOpType    roqOpType         rfWen
    //   |            |            |            |           |           |                 |  fpWen
    //   |            |            |            |           |           |                 |  |  isXSTrap
    //   |            |            |            |           |           |                 |  |  |  flushPipe
    //   |            |            |            |           |           |                 |  |  |  |  isRVF
    //   |            |            |            |           |           |                 |  |  |  |  |  selImm
    List(SrcType.DC, SrcType.DC, SrcType.DC, FuType.alu, ALUOpType.sll, RoqOpType.NORMAL, N, N, N, N, N, SelImm.INVALID_INSTR) // Use SelImm to indicate invalid instr
  
    val table: Array[(BitPat, List[BitPat])]
}

trait DecodeUnitConstants
{
  // abstract out instruction decode magic numbers
  val RD_MSB  = 11
  val RD_LSB  = 7
  val RS1_MSB = 19
  val RS1_LSB = 15
  val RS2_MSB = 24
  val RS2_LSB = 20
  val RS3_MSB = 31
  val RS3_LSB = 27
}

/**
 * Decoded control signals
 * See xiangshan/package.scala, xiangshan/backend/package.scala, Bundle.scala
 */

/**
 * Decode constants for RV64
 */
object X64Decode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
    LD      -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.ld, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),
    LWU     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lwu, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),
    SD      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.stu, LSUOpType.sd, RoqOpType.NORMAL, N, N, N, N, N, SelImm.IMM_S),

    SLLI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.sll, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),
    SRLI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.srl, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),
    SRAI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.sra, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),

    ADDIW   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.addw, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),
    SLLIW   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.sllw, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),
    SRAIW   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.sraw, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),
    SRLIW   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.srlw, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),

    ADDW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.addw, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    SUBW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.subw, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    SLLW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sllw, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    SRAW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sraw, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    SRLW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.srlw, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X)
  )
}

/**
 * Overall Decode constants
 */
object XDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
    LW      -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lw, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),
    LH      -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lh, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),
    LHU     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lhu, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),
    LB      -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lb, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),
    LBU     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lbu, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),

    SW      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.stu, LSUOpType.sw, RoqOpType.NORMAL, N, N, N, N, N, SelImm.IMM_S),
    SH      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.stu, LSUOpType.sh, RoqOpType.NORMAL, N, N, N, N, N, SelImm.IMM_S),
    SB      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.stu, LSUOpType.sb, RoqOpType.NORMAL, N, N, N, N, N, SelImm.IMM_S),

    LUI     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.add, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_U),

    ADDI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.add, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),
    ANDI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.and, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),
    ORI     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.or, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),
    XORI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.xor, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),
    SLTI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.slt, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),
    SLTIU   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.sltu, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),

    SLL     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sll, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    ADD     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.add, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    SUB     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sub, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    SLT     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.slt, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    SLTU    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sltu, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    AND     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.and, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    OR      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.or, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    XOR     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.xor, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    SRA     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sra, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    SRL     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.srl, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),

    MUL     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mul, MDUOpType.mul, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    MULH    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mul, MDUOpType.mulh, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    MULHU   -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mul, MDUOpType.mulhu, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    MULHSU  -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mul, MDUOpType.mulhsu, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    MULW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mul, MDUOpType.mulw, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),

    DIV     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.div, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    DIVU    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.divu, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    REM     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.rem, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    REMU    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.remu, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    DIVW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.divw, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    DIVUW   -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.divuw, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    REMW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.remw, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
    REMUW   -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.remuw, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),

    AUIPC   -> List(SrcType.pc, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.add, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_U),
    JAL     -> List(SrcType.pc , SrcType.imm, SrcType.DC, FuType.jmp, JumpOpType.jal, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_UJ),
    JALR    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.jmp, JumpOpType.jalr, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_I),
    BEQ     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.beq, RoqOpType.NORMAL, N, N, N, N, N, SelImm.IMM_SB),
    BNE     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.bne, RoqOpType.NORMAL, N, N, N, N, N, SelImm.IMM_SB),
    BGE     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.bge, RoqOpType.NORMAL, N, N, N, N, N, SelImm.IMM_SB),
    BGEU    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.bgeu, RoqOpType.NORMAL, N, N, N, N, N, SelImm.IMM_SB),
    BLT     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.blt, RoqOpType.NORMAL, N, N, N, N, N, SelImm.IMM_SB),
    BLTU    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.bltu, RoqOpType.NORMAL, N, N, N, N, N, SelImm.IMM_SB),

    // I-type, the immediate12 holds the CSR register.
    CSRRW   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.wrt, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_I),
    CSRRS   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.set, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_I),
    CSRRC   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.clr, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_I),

    CSRRWI  -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.wrti, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_I),
    CSRRSI  -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.seti, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_I),
    CSRRCI  -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.clri, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_I),

    SFENCE_VMA->List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.fence, FenceOpType.sfence, RoqOpType.BLOCK, N, N, N, Y, N, SelImm.IMM_X),
    ECALL   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.jmp, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    SRET    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.jmp, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    MRET    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.jmp, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),

    WFI     -> List(SrcType.pc, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.wfi, RoqOpType.WFI, Y, N, N, N, N, SelImm.IMM_X),

    FENCE_I -> List(SrcType.pc, SrcType.imm, SrcType.DC, FuType.fence, FenceOpType.fencei, RoqOpType.BLOCK, N, N, N, Y, N, SelImm.IMM_X),
    FENCE   -> List(SrcType.pc, SrcType.imm, SrcType.DC, FuType.fence, FenceOpType.fence, RoqOpType.BLOCK, N, N, N, Y, N, SelImm.IMM_X),

    // A-type
    AMOADD_W-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoadd_w, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    AMOXOR_W-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoxor_w, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    AMOSWAP_W->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoswap_w, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    AMOAND_W-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoand_w, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    AMOOR_W -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoor_w, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    AMOMIN_W-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomin_w, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    AMOMINU_W->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amominu_w, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    AMOMAX_W-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomax_w, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    AMOMAXU_W->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomaxu_w, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),

    AMOADD_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoadd_d, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    AMOXOR_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoxor_d, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    AMOSWAP_D->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoswap_d, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    AMOAND_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoand_d, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    AMOOR_D -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoor_d, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    AMOMIN_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomin_d, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    AMOMINU_D->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amominu_d, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    AMOMAX_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomax_d, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    AMOMAXU_D->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomaxu_d, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),

    LR_W    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.mou, LSUOpType.lr_w, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    LR_D    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.mou, LSUOpType.lr_d, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    SC_W    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.sc_w, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X),
    SC_D    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.sc_d, RoqOpType.BLOCK, Y, N, N, N, N, SelImm.IMM_X)
  )
}

/**
 * FP Decode constants
 */
object FDecode extends DecodeConstants{
  val table: Array[(BitPat, List[BitPat])] = Array(

  FLW     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.flw, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_I),
  FLD     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.ld, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_I),
  FSW     -> List(SrcType.reg, SrcType.fp, SrcType.DC, FuType.stu, LSUOpType.sw, RoqOpType.NORMAL, N, N, N, N, Y, SelImm.IMM_S),
  FSD     -> List(SrcType.reg, SrcType.fp, SrcType.DC, FuType.stu, LSUOpType.sd, RoqOpType.NORMAL, N, N, N, N, N, SelImm.IMM_S),

  FCLASS_S-> List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, FPUOpType.fclass, RoqOpType.NORMAL, Y, N, N, N, Y, SelImm.IMM_X),
  FCLASS_D-> List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, FPUOpType.fclass, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),

  FMV_D_X -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.fmv_i2f, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),
  FMV_X_D -> List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, FPUOpType.fmv_f2i, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
  FMV_X_W -> List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, FPUOpType.fmv_f2i, RoqOpType.NORMAL, Y, N, N, N, Y, SelImm.IMM_X),
  FMV_W_X -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.fmv_i2f, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),

  FSGNJ_S -> List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fsgnj, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FSGNJ_D -> List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fsgnj, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),
  FSGNJX_S-> List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fsgnjx, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FSGNJX_D-> List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fsgnjx, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),
  FSGNJN_S-> List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fsgnjn, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FSGNJN_D-> List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fsgnjn, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),

  // FP to FP
  FCVT_S_D-> List(SrcType.fp, SrcType.imm, SrcType.DC, FuType.fmisc, FPUOpType.d2s, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FCVT_D_S-> List(SrcType.fp, SrcType.imm, SrcType.DC, FuType.fmisc, FPUOpType.s2d, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),

  // Int to FP
  FCVT_S_W-> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.w2f, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FCVT_S_WU->List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.wu2f, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FCVT_S_L-> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.l2f, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FCVT_S_LU->List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.lu2f, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),

  FCVT_D_W-> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.w2f, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),
  FCVT_D_WU->List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.wu2f, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),
  FCVT_D_L-> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.l2f, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),
  FCVT_D_LU->List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.lu2f, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),

  // FP to Int
  FCVT_W_S-> List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, FPUOpType.f2w, RoqOpType.NORMAL, Y, N, N, N, Y, SelImm.IMM_X),
  FCVT_WU_S->List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, FPUOpType.f2wu, RoqOpType.NORMAL, Y, N, N, N, Y, SelImm.IMM_X),
  FCVT_L_S-> List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, FPUOpType.f2l, RoqOpType.NORMAL, Y, N, N, N, Y, SelImm.IMM_X),
  FCVT_LU_S->List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, FPUOpType.f2lu, RoqOpType.NORMAL, Y, N, N, N, Y, SelImm.IMM_X),

  FCVT_W_D-> List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, FPUOpType.f2w, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
  FCVT_WU_D->List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, FPUOpType.f2wu, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
  FCVT_L_D-> List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, FPUOpType.f2l, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
  FCVT_LU_D->List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, FPUOpType.f2lu, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),

  // "fp_single" is used for wb_data formatting (and debugging)
  FEQ_S    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.feq, RoqOpType.NORMAL, Y, N, N, N, Y, SelImm.IMM_X),
  FLT_S    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.flt, RoqOpType.NORMAL, Y, N, N, N, Y, SelImm.IMM_X),
  FLE_S    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fle, RoqOpType.NORMAL, Y, N, N, N, Y, SelImm.IMM_X),

  FEQ_D    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.feq, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
  FLT_D    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.flt, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),
  FLE_D    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fle, RoqOpType.NORMAL, Y, N, N, N, N, SelImm.IMM_X),

  FMIN_S   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fmin, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FMAX_S   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fmax, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FMIN_D   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fmin, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),
  FMAX_D   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fmax, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),

  FADD_S   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, FPUOpType.fadd, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FSUB_S   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, FPUOpType.fsub, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FMUL_S   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, FPUOpType.fmul, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FADD_D   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, FPUOpType.fadd, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),
  FSUB_D   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, FPUOpType.fsub, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),
  FMUL_D   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, FPUOpType.fmul, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),

  FMADD_S  ->List(SrcType.fp,  SrcType.fp, SrcType.fp, FuType.fmac, FPUOpType.fmadd, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FMSUB_S  ->List(SrcType.fp,  SrcType.fp, SrcType.fp, FuType.fmac, FPUOpType.fmsub, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FNMADD_S ->List(SrcType.fp,  SrcType.fp, SrcType.fp, FuType.fmac, FPUOpType.fnmadd, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FNMSUB_S ->List(SrcType.fp,  SrcType.fp, SrcType.fp, FuType.fmac, FPUOpType.fnmsub, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FMADD_D  ->List(SrcType.fp,  SrcType.fp, SrcType.fp, FuType.fmac, FPUOpType.fmadd, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),
  FMSUB_D  ->List(SrcType.fp,  SrcType.fp, SrcType.fp, FuType.fmac, FPUOpType.fmsub, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),
  FNMADD_D ->List(SrcType.fp,  SrcType.fp, SrcType.fp, FuType.fmac, FPUOpType.fnmadd, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),
  FNMSUB_D ->List(SrcType.fp,  SrcType.fp, SrcType.fp, FuType.fmac, FPUOpType.fnmsub, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X)
  )
}

/**
 * FP Divide SquareRoot Constants
 */
object FDivSqrtDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
  FDIV_S    ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fdiv, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FDIV_D    ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fdiv, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X),
  FSQRT_S   ->List(SrcType.fp,  SrcType.imm, SrcType.DC, FuType.fmisc, FPUOpType.fsqrt, RoqOpType.NORMAL, N, Y, N, N, Y, SelImm.IMM_X),
  FSQRT_D   ->List(SrcType.fp,  SrcType.imm, SrcType.DC, FuType.fmisc, FPUOpType.fsqrt, RoqOpType.NORMAL, N, Y, N, N, N, SelImm.IMM_X)
  )
}

/**
 * XiangShan Trap Decode constants
 */
object XSTrapDecode extends DecodeConstants {
  // calculate as ADDI => addi zero, a0, 0
  // replace rs '?????' with '01010'(a0) in decode stage
  def lsrc1 = "b01010".U // $a0
  val table: Array[(BitPat, List[BitPat])] = Array(
    TRAP    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.add, RoqOpType.BLOCK, Y, N, Y, N, N, SelImm.IMM_I)
  )
}

class RVCExpander extends XSModule {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Output(new ExpandedInstruction)
    val rvc = Output(Bool())
  })

  if (HasCExtension) {
    io.rvc := io.in(1,0) =/= 3.U
    io.out := new RVCDecoder(io.in, XLEN).decode
  } else {
    io.rvc := false.B
    io.out := new RVCDecoder(io.in, XLEN).passthrough
  }
}

object Imm32Gen {
  def apply(sel: UInt, inst: UInt) = {
    val sign = Mux(sel === SelImm.IMM_Z, 0.S, inst(31).asSInt)
    val b30_20 = Mux(sel === SelImm.IMM_U, inst(30,20).asSInt, sign)
    val b19_12 = Mux(sel =/= SelImm.IMM_U && sel =/= SelImm.IMM_UJ, sign, inst(19,12).asSInt)
    val b11 = Mux(sel === SelImm.IMM_U || sel === SelImm.IMM_Z, 0.S,
              Mux(sel === SelImm.IMM_UJ, inst(20).asSInt,
              Mux(sel === SelImm.IMM_SB, inst(7).asSInt, sign)))
    val b10_5 = Mux(sel === SelImm.IMM_U || sel === SelImm.IMM_Z, 0.U(1.W), inst(30,25))
    val b4_1 = Mux(sel === SelImm.IMM_U, 0.U(1.W),
               Mux(sel === SelImm.IMM_S || sel === SelImm.IMM_SB, inst(11,8),
               Mux(sel === SelImm.IMM_Z, inst(19,16), inst(24,21))))
    val b0 = Mux(sel === SelImm.IMM_S, inst(7),
             Mux(sel === SelImm.IMM_I, inst(20),
             Mux(sel === SelImm.IMM_Z, inst(15), 0.U(1.W))))

    Cat(sign, b30_20, b19_12, b11, b10_5, b4_1, b0)
  }
}

/**
 * IO bundle for the Decode unit
 */
class DecodeUnitIO extends XSBundle {
  val enq = new Bundle { val ctrl_flow = Input(new CtrlFlow) }
  val deq = new Bundle { val cf_ctrl = Output(new CfCtrl) }
}

/**
 * Decode unit that takes in a single CtrlFlow and generates a CfCtrl.
 */
class DecodeUnit extends XSModule with DecodeUnitConstants {
  val io = IO(new DecodeUnitIO)

  val ctrl_flow = Wire(new CtrlFlow) // input with RVC Expanded
  val cf_ctrl = Wire(new CfCtrl)

  val exp = Module(new RVCExpander())
  exp.io.in := io.enq.ctrl_flow.instr
  ctrl_flow := io.enq.ctrl_flow
  when (exp.io.rvc) {
    ctrl_flow.instr := exp.io.out.bits
  }

  // save rvc decode info
  // TODO maybe rvc_info are useless?
  val rvc_info = Wire(new ExpandedInstruction())
  val is_rvc = Wire(Bool())
  rvc_info := exp.io.out
  is_rvc := exp.io.rvc

  var decode_table = XDecode.table ++ FDecode.table ++ FDivSqrtDecode.table ++ X64Decode.table ++ XSTrapDecode.table

  // output
  cf_ctrl.cf := ctrl_flow
  cf_ctrl.brTag := DontCare
  val cs = Wire(new CtrlSignals()).decode(ctrl_flow.instr, decode_table)

  // read src1~3 location
  cs.lsrc1 := Mux(ctrl_flow.instr === LUI || cs.src1Type === SrcType.pc, 0.U, ctrl_flow.instr(RS1_MSB,RS1_LSB))
  cs.lsrc2 := ctrl_flow.instr(RS2_MSB,RS2_LSB)
  cs.lsrc3 := ctrl_flow.instr(RS3_MSB,RS3_LSB)
  // read dest location
  cs.ldest := Mux(cs.fpWen || cs.rfWen, ctrl_flow.instr(RD_MSB,RD_LSB), 0.U)

  // fill in exception vector
  cf_ctrl.cf.exceptionVec.map(_ := false.B)
  cf_ctrl.cf.exceptionVec(illegalInstr) := cs.selImm === SelImm.INVALID_INSTR
  cf_ctrl.cf.exceptionVec(instrPageFault) := io.enq.ctrl_flow.exceptionVec(instrPageFault)
  cf_ctrl.cf.exceptionVec(instrAccessFault) := io.enq.ctrl_flow.exceptionVec(instrAccessFault)
  
  // fix frflags
  //                           fflags    zero csrrs rd    csr
  val isFrflags = BitPat("b000000000001_00000_010_?????_1110011") === ctrl_flow.instr
  when (cs.fuType === FuType.csr && isFrflags) {
    cs.roqOpType := RoqOpType.NOSPEC
  }

  // fix isXSTrap
  when (cs.isXSTrap) {
    cs.lsrc1 := XSTrapDecode.lsrc1
  }

  cs.imm := SignExt(Imm32Gen(cs.selImm, ctrl_flow.instr), XLEN)

  cf_ctrl.ctrl := cs

  // fix ret and call
  when (cs.fuType === FuType.jmp) {
    def isLink(reg: UInt) = (reg === 1.U || reg === 5.U)
    when (isLink(cs.ldest) && cs.fuOpType === JumpOpType.jal) { cf_ctrl.ctrl.fuOpType := JumpOpType.call }
    when (cs.fuOpType === JumpOpType.jalr) {
      when (isLink(cs.lsrc1)) { cf_ctrl.ctrl.fuOpType := JumpOpType.ret  }
      when (isLink(cs.ldest)) { cf_ctrl.ctrl.fuOpType := JumpOpType.call }
    }
  }

  io.deq.cf_ctrl := cf_ctrl

  //-------------------------------------------------------------
  // Debug Info
  XSDebug("in:  instr=%x pc=%x excepVec=%b intrVec=%b crossPageIPFFix=%d\n",
    io.enq.ctrl_flow.instr, io.enq.ctrl_flow.pc, io.enq.ctrl_flow.exceptionVec.asUInt,
    io.enq.ctrl_flow.intrVec.asUInt, io.enq.ctrl_flow.crossPageIPFFix)
  XSDebug("out: src1Type=%b src2Type=%b src3Type=%b lsrc1=%d lsrc2=%d lsrc3=%d ldest=%d fuType=%b fuOpType=%b\n",
    io.deq.cf_ctrl.ctrl.src1Type, io.deq.cf_ctrl.ctrl.src2Type, io.deq.cf_ctrl.ctrl.src3Type,
    io.deq.cf_ctrl.ctrl.lsrc1, io.deq.cf_ctrl.ctrl.lsrc2, io.deq.cf_ctrl.ctrl.lsrc3,
    io.deq.cf_ctrl.ctrl.ldest, io.deq.cf_ctrl.ctrl.fuType, io.deq.cf_ctrl.ctrl.fuOpType)
  XSDebug("out: rfWen=%d fpWen=%d isXSTrap=%d roqOpType=%d flushPipe=%d isRVF=%d imm=%x\n",
    io.deq.cf_ctrl.ctrl.rfWen, io.deq.cf_ctrl.ctrl.fpWen, io.deq.cf_ctrl.ctrl.isXSTrap,
    io.deq.cf_ctrl.ctrl.roqOpType, io.deq.cf_ctrl.ctrl.flushPipe,
    io.deq.cf_ctrl.ctrl.isRVF, io.deq.cf_ctrl.ctrl.imm)
  XSDebug("out: excepVec=%b intrVec=%b\n",
    io.deq.cf_ctrl.cf.exceptionVec.asUInt, io.deq.cf_ctrl.cf.intrVec.asUInt)
}
