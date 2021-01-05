package xiangshan.frontend

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import xiangshan._
import xiangshan.backend.ALUOpType
import utils._
import chisel3.experimental.chiselName


import scala.math.min

trait BTBParams extends HasXSParameter {
  val nRows = BtbSize / (PredictWidth * BtbWays)
  val offsetLen = 13
  val extendedNRows = nRows
}

class BtbDataEntry extends XSBundle with BTBParams {
  val offset = SInt(offsetLen.W)
  val extended = Bool()
}

object BtbDataEntry {
  def apply(offset: SInt, extended: Bool) = {
    val e = Wire(new BtbDataEntry)
    e.offset := offset
    e.extended := extended
    e
  }
}

class BtbMetaEntry() extends XSBundle with BTBParams {
  val valid = Bool()
  // TODO: don't need full length of tag
  val tag = UInt((VAddrBits - log2Up(BtbSize) - 1).W)
  val btbType = UInt(2.W)
  val isRVC = Bool()
}

object BtbMetaEntry {
  def apply(tag: UInt, btbType: UInt, isRVC: Bool) = {
    val e = Wire(new BtbMetaEntry)
    e.valid := true.B
    e.tag := tag
    e.btbType := btbType
    e.isRVC := isRVC
    e
  }
}

class BTB extends BasePredictor with BTBParams{
  class BTBResp extends Resp {
    val targets = Vec(PredictWidth, UInt(VAddrBits.W))
    val hits = Vec(PredictWidth, Bool())
    val types = Vec(PredictWidth, UInt(2.W))
    val isRVC = Vec(PredictWidth, Bool())
  }
  class BTBMeta extends Meta {
    val writeWay =  Vec(PredictWidth, UInt(log2Up(BtbWays).W))
    val hitJal = Vec(PredictWidth, Bool())
  }
  class BTBFromOthers extends FromOthers {}

  class BTBIO extends DefaultBasePredictorIO {
    val resp = Output(new BTBResp)
    val meta = Output(new BTBMeta)
  }
  override val debug = true
  override val io = IO(new BTBIO)
  val btbAddr = new TableAddr(log2Up(BtbSize/BtbWays), BtbBanks)

  val if1_bankAlignedPC = bankAligned(io.pc.bits)

  val if2_pc = RegEnable(if1_bankAlignedPC, io.pc.valid)

  val data = List.fill(BtbWays) {
    List.fill(BtbBanks) {
      Module(new SRAMTemplate(new BtbDataEntry, set = nRows, shouldReset = true, holdRead = true))
    }
  }
  val meta = List.fill(BtbWays) {
    List.fill(BtbBanks) {
      Module(new SRAMTemplate(new BtbMetaEntry, set = nRows, shouldReset = true, holdRead = true))
    }
  }
  val edata = List.fill(2)(Module(new SRAMTemplate(UInt(VAddrBits.W), set = extendedNRows/2, shouldReset = true, holdRead = true)))

  // BTB read requests

  // this bank means cache bank
  val if1_startsAtOddBank = bankInGroup(if1_bankAlignedPC)(0)

  val if1_baseBank = btbAddr.getBank(if1_bankAlignedPC)

  val if1_realMask = Mux(if1_startsAtOddBank,
                      Cat(io.inMask(bankWidth-1,0), io.inMask(PredictWidth-1, bankWidth)),
                      io.inMask)

  val if2_realMask = RegEnable(if1_realMask, io.pc.valid)

  val if1_isInNextRow = VecInit((0 until BtbBanks).map(i => Mux(if1_startsAtOddBank, (i < bankWidth).B, false.B)))

  val if1_baseRow = btbAddr.getBankIdx(if1_bankAlignedPC)

  val if1_nextRowStartsUp = if1_baseRow.andR

  val if1_realRow = VecInit((0 until BtbBanks).map(b => Mux(if1_isInNextRow(b), (if1_baseRow+1.U)(log2Up(nRows)-1, 0), if1_baseRow)))

  val if2_realRow = VecInit(if1_realRow.map(RegEnable(_, enable=io.pc.valid)))

  for (w <- 0 until BtbWays) {
    for (b <- 0 until BtbBanks) {
      meta(w)(b).io.r.req.valid       := if1_realMask(b) && io.pc.valid
      meta(w)(b).io.r.req.bits.setIdx := if1_realRow(b)
      data(w)(b).io.r.req.valid       := if1_realMask(b) && io.pc.valid
      data(w)(b).io.r.req.bits.setIdx := if1_realRow(b)
    }
  }
  for (b <- 0 to 1) {
    edata(b).io.r.req.valid       := io.pc.valid
    val row = if (b == 0) { Mux(if1_startsAtOddBank, if1_realRow(bankWidth), if1_realRow(0)) }
              else { Mux(if1_startsAtOddBank, if1_realRow(0), if1_realRow(bankWidth))}
    edata(b).io.r.req.bits.setIdx := row
  }

  // Entries read from SRAM
  val if2_metaRead = VecInit((0 until BtbWays).map(w => VecInit((0 until BtbBanks).map( b => meta(w)(b).io.r.resp.data(0)))))
  val if2_dataRead = VecInit((0 until BtbWays).map(w => VecInit((0 until BtbBanks).map( b => data(w)(b).io.r.resp.data(0)))))
  val if2_edataRead = VecInit((0 to 1).map(i => edata(i).io.r.resp.data(0)))

  val if2_baseBank = btbAddr.getBank(if2_pc)
  val if2_startsAtOddBank = bankInGroup(if2_pc)(0)
  val if2_baseTag = btbAddr.getTag(if2_pc)

  val if2_tagIncremented = VecInit((0 until BtbBanks).map(b => RegEnable(if1_isInNextRow(b.U) && if1_nextRowStartsUp, io.pc.valid)))
  val if2_realTags = VecInit((0 until BtbBanks).map(b => Mux(if2_tagIncremented(b), if2_baseTag + 1.U, if2_baseTag)))

  val if2_totalHits = VecInit((0 until BtbBanks).map( b =>
    VecInit((0 until BtbWays).map( w =>
      // This should correspond to the real mask from last valid cycle!
      if2_metaRead(w)(b).tag === if2_realTags(b) && if2_metaRead(w)(b).valid && if2_realMask(b)
    ))
  ))
  val if2_bankHits = VecInit(if2_totalHits.map(_.reduce(_||_)))
  val if2_bankHitWays = VecInit(if2_totalHits.map(PriorityEncoder(_)))


  def allocWay(valids: UInt, meta_tags: UInt, req_tag: UInt) = {
    val randomAlloc = true
    if (BtbWays > 1) {
      val w = Wire(UInt(log2Up(BtbWays).W))
      val valid = WireInit(valids.andR)
      val tags = Cat(meta_tags, req_tag)
      val l = log2Up(BtbWays)
      val nChunks = (tags.getWidth + l - 1) / l
      val chunks = (0 until nChunks).map( i =>
        tags(min((i+1)*l, tags.getWidth)-1, i*l)
      )
      w := Mux(valid, if (randomAlloc) {LFSR64()(log2Up(BtbWays)-1,0)} else {chunks.reduce(_^_)}, PriorityEncoder(~valids))
      w
    } else {
      val w = WireInit(0.U)
      w
    }
  }
  val allocWays = VecInit((0 until BtbBanks).map(b =>
    allocWay(VecInit(if2_metaRead.map(w => w(b).valid)).asUInt,
             VecInit(if2_metaRead.map(w => w(b).tag)).asUInt,
             if2_realTags(b))))

  val writeWay = VecInit((0 until BtbBanks).map(
    b => Mux(if2_bankHits(b), if2_bankHitWays(b), allocWays(b))
  ))



  for (b <- 0 until BtbBanks) {
    val realBank = (if (b < bankWidth) Mux(if2_startsAtOddBank, (b+bankWidth).U, b.U)
                    else Mux(if2_startsAtOddBank, (b-bankWidth).U, b.U))
    val meta_entry = if2_metaRead(if2_bankHitWays(realBank))(realBank)
    val data_entry = if2_dataRead(if2_bankHitWays(realBank))(realBank)
    val edataBank = (if (b < bankWidth) Mux(if2_startsAtOddBank, 1.U, 0.U)
                     else Mux(if2_startsAtOddBank, 0.U, 1.U))
    // Use real pc to calculate the target
    io.resp.targets(b) := Mux(data_entry.extended, if2_edataRead(edataBank), (if2_pc.asSInt + (b << 1).S + data_entry.offset).asUInt)
    io.resp.hits(b)  := if2_bankHits(realBank)
    io.resp.types(b) := meta_entry.btbType
    io.resp.isRVC(b) := meta_entry.isRVC
    io.meta.writeWay(b) := writeWay(realBank)
    io.meta.hitJal(b)   := if2_bankHits(realBank) && meta_entry.btbType === BTBtype.J
  }

  def pdInfoToBTBtype(pd: PreDecodeInfo) = {
    val t = WireInit(0.U(2.W))
    when (pd.isJalr) { t := BTBtype.I}
    when (pd.isRet)  { t := BTBtype.R}
    when (pd.isJal)  { t := BTBtype.J}
    when (pd.isBr)   { t := BTBtype.B}
    t
  }
  val u = io.update.bits

  val max_offset = Cat(0.B, ~(0.U((offsetLen-1).W))).asSInt
  val min_offset = Cat(1.B,  (0.U((offsetLen-1).W))).asSInt
  val new_target = Mux(u.pd.isBr, u.brTarget, u.target)
  val new_offset = (new_target.asSInt - u.pc.asSInt)
  val new_extended = (new_offset > max_offset || new_offset < min_offset)


  val updateWay = u.bpuMeta.btbWriteWay
  val updateBankIdx = btbAddr.getBank(u.pc)
  val updateEBank = updateBankIdx(log2Ceil(BtbBanks)-1) // highest bit of bank idx
  val updateRow = btbAddr.getBankIdx(u.pc)
  val updateType = pdInfoToBTBtype(u.pd)
  val metaWrite = BtbMetaEntry(btbAddr.getTag(u.pc), updateType, u.pd.isRVC)
  val dataWrite = BtbDataEntry(new_offset, new_extended)

  val jalFirstEncountered = !u.isMisPred && !u.bpuMeta.btbHitJal && updateType === BTBtype.J
  val updateValid = io.update.valid && (u.isMisPred || jalFirstEncountered) && !u.isReplay
  // Update btb
  for (w <- 0 until BtbWays) {
    for (b <- 0 until BtbBanks) {
      meta(w)(b).io.w.req.valid := updateValid && b.U === updateBankIdx && w.U === updateWay
      meta(w)(b).io.w.req.bits.setIdx := updateRow
      meta(w)(b).io.w.req.bits.data := metaWrite
      data(w)(b).io.w.req.valid := updateValid && b.U === updateBankIdx && w.U === updateWay
      data(w)(b).io.w.req.bits.setIdx := updateRow
      data(w)(b).io.w.req.bits.data := dataWrite
    }
  }

  for (b <- 0 to 1) {
    edata(b).io.w.req.valid := updateValid && new_extended && b.U === updateEBank
    edata(b).io.w.req.bits.setIdx := updateRow
    edata(b).io.w.req.bits.data := u.target
  }


  if (BPUDebug && debug) {
    val debug_verbose = true

    XSDebug("isInNextRow: ")
    (0 until BtbBanks).foreach(i => {
      XSDebug(false, true.B, "%d ", if1_isInNextRow(i))
      if (i == BtbBanks-1) { XSDebug(false, true.B, "\n") }
    })

    val validLatch = RegNext(io.pc.valid)
    XSDebug(io.pc.valid, "read: pc=0x%x, baseBank=%d, realMask=%b\n", if1_bankAlignedPC, if1_baseBank, if1_realMask)
    XSDebug(validLatch, "read_resp: pc=0x%x, readIdx=%d-------------------------------\n",
      if2_pc, btbAddr.getIdx(if2_pc))
    if (debug_verbose) {
      for (i <- 0 until BtbBanks){
        for (j <- 0 until BtbWays) {
          XSDebug(validLatch, "read_resp[w=%d][b=%d][r=%d] is valid(%d) mask(%d), tag=0x%x, offset=0x%x, type=%d, isExtend=%d, isRVC=%d\n",
          j.U, i.U, if2_realRow(i), if2_metaRead(j)(i).valid, if2_realMask(i), if2_metaRead(j)(i).tag, if2_dataRead(j)(i).offset, if2_metaRead(j)(i).btbType, if2_dataRead(j)(i).extended, if2_metaRead(j)(i).isRVC)
        }
      }
    }
    // e.g: baseBank == 5 => (5, 6,..., 15, 0, 1, 2, 3, 4)
    val bankIdxInOrder = VecInit((0 until BtbBanks).map(b => (if2_baseBank +& b.U)(log2Up(BtbBanks)-1,0)))

    for (i <- 0 until BtbBanks) {
      val idx = bankIdxInOrder(i)
      XSDebug(validLatch && if2_bankHits(bankIdxInOrder(i)), "resp(%d): bank(%d) hits, tgt=%x, isRVC=%d, type=%d\n",
        i.U, idx, io.resp.targets(i), io.resp.isRVC(i), io.resp.types(i))
    }
    XSDebug(updateValid, "update_req: cycle=%d, pc=0x%x, target=0x%x, misPred=%d, offset=%x, extended=%d, way=%d, bank=%d, row=0x%x\n",
      u.bpuMeta.debug_btb_cycle, u.pc, new_target, u.isMisPred, new_offset, new_extended, updateWay, updateBankIdx, updateRow)
    for (i <- 0 until BtbBanks) {
      // Conflict when not hit and allocating a valid entry
      val conflict = if2_metaRead(allocWays(i))(i).valid && !if2_bankHits(i)
      XSDebug(conflict, "bank(%d) is trying to allocate a valid way(%d)\n", i.U, allocWays(i))
      // There is another circumstance when a branch is on its way to update while another
      // branch chose the same way to udpate, then after the first branch is wrote in,
      // the second branch will overwrite the first branch
  }

  }
}