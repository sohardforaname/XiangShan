package xiangshan.mem

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.cache._

trait HasSbufferCst extends HasXSParameter {

  def s_invalid = 0.U(2.W)
  def s_valid = 1.U(2.W)
  def s_prepare = 2.U(2.W)
  def s_inflight = 3.U(2.W)

  val SbufferIndexWidth: Int = log2Up(StoreBufferSize)
  // paddr = tag + offset
  val CacheLineBytes: Int = CacheLineSize / 8
  val CacheLineWords: Int = CacheLineBytes / DataBytes
  val OffsetWidth: Int = log2Up(CacheLineBytes)
  val WordsWidth: Int = log2Up(CacheLineWords)
  val TagWidth: Int = PAddrBits - OffsetWidth
}

class SbufferBundle extends XSBundle with HasSbufferCst

class SbufferLine extends SbufferBundle {
  val tag = UInt(TagWidth.W)
  val data = UInt(CacheLineSize.W)
  val mask = UInt(CacheLineBytes.W)

  override def toPrintable: Printable = {
    p"tag:${Hexadecimal(tag)} data:${Hexadecimal(data)} mask:${Binary(mask)}\n"
  }
}

class ChooseReplace(nWay: Int) extends XSModule {
  val io = IO(new Bundle{
    val mask = Vec(nWay, Input(Bool()))
    val fire = Input(Bool())
    val way = Output(UInt(nWay.W))
    val flush = Input(Bool())
  })
  val wayReg = RegInit(0.U(log2Up(nWay).W))
  val wayMask = ~((UIntToOH(wayReg)<<1.U)(nWay-1,0) - 1.U)
  val stateMask = Cat(io.mask.reverse)
  val loMask = (wayMask & stateMask)(nWay-1,0)

  val nextWay = PriorityEncoder(Cat(stateMask, loMask))(log2Up(nWay)-1, 0)
  XSDebug(p"nextWay[${nextWay}]\n")

  io.way := wayReg

  when(io.fire){
    wayReg := nextWay
  }

  when(io.flush){
    wayReg := 0.U
  }
}

class SbufferLru(nWay: Int) extends XSModule {
  val io = IO(new Bundle{
    val in = Vec(StorePipelineWidth, Input(UInt(nWay.W)))
    val mask = Vec(StoreBufferSize, Input(Bool()))
    val way = Output(UInt(nWay.W))
    val flush = Input(Bool())
  })

  val lruRect = RegInit(VecInit(Seq.fill(StoreBufferSize)(0.U(nWay.W))))
  val count = RegInit(VecInit(Seq.fill(StoreBufferSize)(0.U(log2Up(nWay+1).W))))
  val idx = RegInit(VecInit(Seq.tabulate(StoreBufferSize)(i => i.U)))

  //update
  val updataMask = ParallelOR(io.in)
  val updateValue = (~updataMask).asUInt()
  for(i <- 0 until nWay){
    val lruUpdate = Mux(updataMask(i), updateValue, lruRect(i) & updateValue)
    lruRect(i) := lruUpdate
    count(i) := PopCount(lruUpdate)
  }

  // get evictionIdx
  val maskCount = Wire(Vec(StoreBufferSize, UInt((log2Up(1 + nWay) + log2Up(nWay)).W)))    // (popcount, Idx)
  val countZipIdx = maskCount.zip((0 until nWay).map(_.U))
  for(i <- 0 until nWay){
    val value = Mux(io.mask(i), count(i), nWay.U)
    maskCount(i) := Cat(value, idx(i))
  }

  io.way := ParallelMin(maskCount)(log2Up(nWay)-1,0)

  // flush
  when(io.flush){
    for(i <- 0 until nWay){
      lruRect(i) := 0.U
      count(i) := nWay.U
    }
    XSDebug("drain sbuffer finish, flush lru\n")
  }
}



class NewSbuffer extends XSModule with HasSbufferCst {
  val io = IO(new Bundle() {
    val in = Vec(StorePipelineWidth, Flipped(Decoupled(new DCacheWordReq)))  //Todo: store logic only support Width == 2 now
    val dcache = new DCacheLineIO
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
    val flush = new Bundle {
      val valid = Input(Bool())
      val empty = Output(Bool())
    } // sbuffer flush
  })

  val buffer = Mem(StoreBufferSize, new SbufferLine)
  val stateVec = RegInit(VecInit(Seq.fill(StoreBufferSize)(s_invalid)))

  /*
       idle --[flush]--> drian_sbuffer --[buf empty]--> idle
            --[buf full]--> replace --[dcache resp]--> idle
    */
  val x_idle :: x_drain_sbuffer :: x_replace :: Nil = Enum(3)
  val sbuffer_state = RegInit(x_idle)

  // ---------------------- Store Enq Sbuffer ---------------------

  def getTag(pa: UInt): UInt =
    pa(PAddrBits - 1, PAddrBits - TagWidth)

  def getWord(pa: UInt): UInt =
    pa(PAddrBits-1, 3)

  def getWordOffset(pa: UInt): UInt =
    pa(OffsetWidth-1, 3)

  def getAddr(tag: UInt): UInt =
    Cat(tag, 0.U((PAddrBits - TagWidth).W))

  def getByteOffset(offect: UInt): UInt =
    Cat(offect(OffsetWidth - 1, 3), 0.U(3.W))

  def isOneOf(key: UInt, seq: Seq[UInt]): Bool =
    if(seq.isEmpty) false.B else Cat(seq.map(_===key)).orR()

  def widthMap[T <: Data](f: Int => T) = (0 until StoreBufferSize) map f

  // sbuffer entry count
  val invalidCount = RegInit(StoreBufferSize.U((log2Up(StoreBufferSize) + 1).W))
  val validCount = RegInit(0.U((log2Up(StoreBufferSize) + 1).W))
  val full = invalidCount === 0.U
  // val oneSpace = invalidCount === 1.U

  val bufferRead  = VecInit((0 until StoreBufferSize).map(i => buffer(i)))
  val stateRead   = VecInit((0 until StoreBufferSize).map(i => stateVec(i)))
  val dataRead    = VecInit((0 until StoreBufferSize).map(i => bufferRead(i).data.asTypeOf(Vec(CacheLineWords, Vec(DataBytes, UInt(8.W))))))
  val maskRead    = VecInit((0 until StoreBufferSize).map(i => bufferRead(i).mask.asTypeOf(Vec(CacheLineWords, Vec(DataBytes, Bool())))))
  val tagRead     = VecInit((0 until StoreBufferSize).map(i => bufferRead(i).tag))

  val dataUpdate  = WireInit(dataRead)
  val maskUpdate  = WireInit(maskRead)
  val tagUpdate   = WireInit(tagRead)
  val stateUpdate = WireInit(stateRead)
  val bufferUpdate = Wire(Vec(StoreBufferSize, new SbufferLine))
  (0 until StoreBufferSize) foreach { i =>
    bufferUpdate(i).tag := tagUpdate(i)
    bufferUpdate(i).data := dataUpdate(i).asUInt()
    bufferUpdate(i).mask := maskUpdate(i).asUInt()
  }

  val lru = Module(new ChooseReplace(StoreBufferSize))
  val evictionIdx = lru.io.way
  
  lru.io.fire := false.B
  lru.io.mask := stateRead.map(_ === s_valid)

  val tags = io.in.map(in => getTag(in.bits.addr))
  val sameTag = tags(0) === tags(1)
  val firstWord = getWord(io.in(0).bits.addr)
  val secondWord = getWord(io.in(1).bits.addr)
  val sameWord = firstWord === secondWord


  // merge condition
  val mergeMask = Wire(Vec(StorePipelineWidth, Vec(StoreBufferSize, Bool())))
  val mergeIdx = mergeMask.map(PriorityEncoder(_))
  val canMerge = mergeMask.map(ParallelOR(_))

  for(i <- 0 until StorePipelineWidth){
    mergeMask(i) := widthMap(j =>
      Mux(tags(i) === tagRead(j) && stateRead(j) === s_valid, true.B, false.B))
  }

  // insert confition
  // firstInsert: the first invalid entry
  // if first entry canMerge or second entry has the same tag with the first entry , secondInsert equal the first invalid entry, otherwise, the second invalid entry
  val invalidMask = stateRead.map(s => s === s_invalid)
  val firstInsertMask = PriorityEncoderOH(invalidMask)
  val secondInsertMask = Wire(Vec(StoreBufferSize, Bool()))
  for (i <- 0 until StoreBufferSize){
    secondInsertMask(i) := Mux(canMerge(0) || sameTag, firstInsertMask(i), invalidMask(i) - firstInsertMask(i))
  }

  val (firstInsertIdx, firstCanInsert) = PriorityEncoderWithFlag(invalidMask)
  val (secondInsertIdx, secondCanInsert) = PriorityEncoderWithFlag(secondInsertMask)

  io.in(0).ready := firstCanInsert || canMerge(0)
  io.in(1).ready := (secondCanInsert || canMerge(1)) && !sameWord && io.in(0).ready


  def wordReqToBufLine(req: DCacheWordReq, tag: UInt, insertIdx: UInt, wordOffset: UInt, flushMask: Bool): Unit = {
    stateUpdate(insertIdx) := s_valid
    tagUpdate(insertIdx) := tag

    when(flushMask){
      for(j <- 0 until CacheLineWords){
        for(i <- 0 until DataBytes){
          maskUpdate(insertIdx)(j)(i) := false.B
        }
      }
    }

    for(i <- 0 until DataBytes){
      when(req.mask(i)){
        maskUpdate(insertIdx)(wordOffset)(i) := true.B
        dataUpdate(insertIdx)(wordOffset)(i) := req.data(i*8+7, i*8)
      }
    }
  }

  def mergeWordReq(req: DCacheWordReq, mergeIdx:UInt, wordOffset:UInt): Unit = {
    for(i <- 0 until DataBytes){
      when(req.mask(i)){
        maskUpdate(mergeIdx)(wordOffset)(i) := true.B
        dataUpdate(mergeIdx)(wordOffset)(i) := req.data(i*8+7, i*8)
      }
    }
  }

  // first store
  when(io.in(0).fire()){
    when(canMerge(0)){
      mergeWordReq(io.in(0).bits, mergeIdx(0), firstWord)
      XSDebug(p"merge req 0 to line [${mergeIdx(0)}]\n")
    }.elsewhen(firstCanInsert){
      wordReqToBufLine(io.in(0).bits, tags(0), firstInsertIdx, firstWord, true.B)
      XSDebug(p"insert req 0 to line[$firstInsertIdx]\n")
    }
  }

  // second store
  when(io.in(1).fire()){
    when(canMerge(1)){
      mergeWordReq(io.in(1).bits, mergeIdx(1), secondWord)
      XSDebug(p"merge req 1 to line [${mergeIdx(1)}]\n")
    }.elsewhen(secondCanInsert){
      wordReqToBufLine(io.in(1).bits, tags(1), secondInsertIdx, secondWord, !sameTag)
      XSDebug(p"insert req 1 to line[$secondInsertIdx]\n")
    }
  }

  for(i <- 0 until StoreBufferSize){
    buffer.write(i.U, bufferUpdate(i))
    stateVec(i) := stateUpdate(i)
  }

  for(i <- 0 until StoreBufferSize){
    XSDebug(stateVec(i)=/=s_invalid,
      p"[$i] state:${stateVec(i)} buf:${bufferRead(i)}\n"
    )
  }

  for((req, i) <- io.in.zipWithIndex){
    XSDebug(req.fire(),
      p"accept req [$i]: " +
        p"addr:${Hexadecimal(req.bits.addr)} " +
        p"mask:${Binary(req.bits.mask)} " +
        p"data:${Hexadecimal(req.bits.data)}\n"
    )
    XSDebug(req.valid && !req.ready,
      p"req [$i] blocked by sbuffer\n"
    )
  }


  // ---------------------- Send Dcache Req ---------------------

  val do_eviction = Wire(Bool())
  val empty = Cat(stateVec.map(s => s===s_invalid)).andR() && !Cat(io.in.map(_.valid)).orR()

  do_eviction := validCount >= 12.U

  io.flush.empty := empty
  lru.io.flush := sbuffer_state === x_drain_sbuffer && empty
  switch(sbuffer_state){
    is(x_idle){
      when(io.flush.valid){
        sbuffer_state := x_drain_sbuffer
      }.elsewhen(do_eviction){
        sbuffer_state := x_replace
      }
    }
    is(x_drain_sbuffer){
      when(empty){
        sbuffer_state := x_idle
      }
    }
    is(x_replace){
      when(io.flush.valid){
        sbuffer_state := x_drain_sbuffer
      }.elsewhen(!do_eviction){
        sbuffer_state := x_idle
      }
    }
  }
  XSDebug(p"sbuffer state:${sbuffer_state} do eviction:${do_eviction} empty:${empty}\n")

  def noSameBlockInflight(idx: UInt): Bool = {
    val tag = tagRead(idx)
    !Cat(widthMap(i => {
      // stateVec(idx) itself must not be s_inflight*
      (stateRead(i) === s_inflight || stateRead(i) === s_prepare) &&
        tag === tagRead(i)
    })).orR()
  }

  /*
      If there is a inflight dcache req which has same tag with evictionIdx's tag,
      current eviction should be blocked.
   */
//  val evictionEntry = Wire(DecoupledIO(UInt(SbufferIndexWidth.W)))
//
//  evictionEntry.valid :=
//    do_eviction && sbuffer_state === x_replace || sbuffer_state === x_drain_sbuffer &&
//      stateVec(evictionIdx)===s_valid &&
//      noSameBlockInflight(evictionIdx)
//
//  evictionEntry.bits := evictionIdx

  val prepareValid = ((do_eviction && sbuffer_state === x_replace)|| (sbuffer_state === x_drain_sbuffer)) &&
                      stateVec(evictionIdx)===s_valid &&
                      noSameBlockInflight(evictionIdx)

  when(prepareValid){
    stateVec(evictionIdx) := s_prepare
    lru.io.fire := true.B
  }


  val prepareMask = stateVec.map(s => s === s_prepare)
  val (prepareIdx, prepareEn) = PriorityEncoderWithFlag(prepareMask)


  io.dcache.req.valid := prepareEn

  io.dcache.req.bits.addr := getAddr(tagRead(prepareIdx))
  io.dcache.req.bits.data := bufferRead(prepareIdx).data
  io.dcache.req.bits.mask := bufferRead(prepareIdx).mask
  io.dcache.req.bits.cmd := MemoryOpConstants.M_XWR
  io.dcache.req.bits.meta := DontCare
  io.dcache.req.bits.meta.id := prepareIdx
  when(io.dcache.req.fire()){
    stateVec(prepareIdx) := s_inflight
  }
//  evictionEntry.ready := io.dcache.req.ready

  XSDebug(io.dcache.req.fire(),
    p"send buf [$prepareIdx] to Dcache, req fire\n"
  )

  io.dcache.resp.ready := true.B // sbuffer always ready to recv dcache resp
  val respId = io.dcache.resp.bits.meta.id
  when(io.dcache.resp.fire()){
    stateVec(respId) := s_invalid
    assert(stateVec(respId) === s_inflight)
    XSDebug(p"recv cache resp: id=[$respId]\n")
  }

  val needSpace = (io.in(0).fire && !canMerge(0)) +& (io.in(1).fire && !canMerge(1) && !sameTag)
  invalidCount := invalidCount - needSpace + io.dcache.resp.fire()
  validCount := validCount + needSpace - prepareValid

  XSDebug(p"needSpace[$needSpace] invalidCount[$invalidCount]  validCount[$validCount]\n")

  // ---------------------- Load Data Forward ---------------------

  for ((forward, i) <- io.forward.zipWithIndex) {
    val tag_matches = widthMap(i => tagRead(i) === getTag(forward.paddr))
    val valid_tag_matches = widthMap(i => tag_matches(i) && stateVec(i) === s_valid)
    val inflight_tag_matches = widthMap(i =>
      tag_matches(i) && (stateVec(i) === s_inflight || stateVec(i) === s_prepare)
    )
    val line_offset_mask = UIntToOH(getWordOffset(forward.paddr))

    val valid_tag_match_reg = valid_tag_matches.map(RegNext(_))
    val inflight_tag_match_reg = inflight_tag_matches.map(RegNext(_))
    val line_offset_reg = RegNext(line_offset_mask)

    val selectedValidLine = Mux1H(valid_tag_match_reg, bufferRead)
    val selectedValidMask = Mux1H(line_offset_reg, selectedValidLine.mask.asTypeOf(Vec(CacheLineWords, Vec(DataBytes, Bool()))))
    val selectedValidData = Mux1H(line_offset_reg, selectedValidLine.data.asTypeOf(Vec(CacheLineWords, Vec(DataBytes, UInt(8.W)))))

    val selectedInflightLine = Mux1H(inflight_tag_match_reg, bufferRead)
    val selectedInflightMask = Mux1H(line_offset_reg, selectedInflightLine.mask.asTypeOf(Vec(CacheLineWords, Vec(DataBytes, Bool()))))
    val selectedInflightData = Mux1H(line_offset_reg, selectedInflightLine.data.asTypeOf(Vec(CacheLineWords, Vec(DataBytes, UInt(8.W)))))

    for (j <- 0 until DataBytes) {
      forward.forwardMask(j) := false.B
      forward.forwardData(j) := DontCare

      // valid entries have higher priority than inflight entries
      when(selectedInflightMask(j)) {
        forward.forwardMask(j) := true.B
        forward.forwardData(j) := selectedInflightData(j)
      }
      when(selectedValidMask(j)) {
        forward.forwardMask(j) := true.B
        forward.forwardData(j) := selectedValidData(j)
      }
    }
  }
}

object NewSbuffer extends App {
  override def main(args: Array[String]): Unit = {
    chisel3.Driver.execute(args, ()=> new NewSbuffer)
  }
}
