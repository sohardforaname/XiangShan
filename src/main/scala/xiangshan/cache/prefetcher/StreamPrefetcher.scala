package xiangshan.cache.prefetcher

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientMetadata
import xiangshan._
import utils._
import scala.math.max

trait StreamPrefetcherParameters {
  def streamCnt: Int
  def streamSize: Int
  def ageWidth: Int
}

case class L1plusPrefetcherParameters (
  streamCnt: Int = 4,
  streamSize: Int = 4,
  ageWidth: Int = 4,
  id: Int = 1
) extends StreamPrefetcherParameters

class StreamBufferEntry extends PrefetcherBundle {
  val req = new PrefetchReq
  val resp = new PrefetchResp
  
  override def toPrintable: Printable = {
    p"0x${Hexadecimal(req.addr)}"
  }
}

class StreamBufferUpdateBundle extends PrefetchReq {
  val hitIdx = UInt(log2Up(streamSize).W) // or one hot?

  override def toPrintable: Printable = {
    p"cmd=${Hexadecimal(cmd)} addr=0x${Hexadecimal(addr)} id=${id} hitIdx=${hitIdx}"
  }
}

class StreamBufferAllocBundle extends PrefetchReq {
  // nothing here
  override def toPrintable: Printable = {
    p"cmd=${Hexadecimal(cmd)} addr=0x${Hexadecimal(addr)} id=${id}"
  }
}

class CompareBundle extends PrefetcherBundle {
  val bits = UInt(ageWidth.W)
  val idx = UInt()
}

object ParallelMin { 
  def apply[T <: Data](xs: Seq[CompareBundle]): CompareBundle = {
    ParallelOperation(xs, (a: CompareBundle, b: CompareBundle) => Mux(a.bits < b.bits, a, b))
  }
}

class StreamBuffer extends PrefetcherModule {
  val io = IO(new Bundle {
    val entryId = Input(UInt(log2Up(streamCnt).W))
    val addrs = Vec(streamSize, ValidIO(UInt(PAddrBits.W)))
    val update = Flipped(ValidIO(new StreamBufferUpdateBundle))
    val alloc = Flipped(ValidIO(new StreamBufferAllocBundle))

    val prefetch = new Bundle {
      val req = DecoupledIO(new PrefetchReq)
      val resp = Flipped(DecoupledIO(new PrefetchResp))
      val finish = DecoupledIO(new PrefetchFinish)
    }
  })

  val baseReq = RegInit(0.U.asTypeOf(Valid(new PrefetchReq)))

  val buf = RegInit(VecInit(Seq.fill(streamSize)(0.U.asTypeOf(new StreamBufferEntry))))
  val valid = RegInit(VecInit(Seq.fill(streamSize)(false.B)))
  val head = RegInit(0.U(log2Up(streamSize).W))
  val tail = RegInit(0.U(log2Up(streamSize).W))
  val full = head === tail && valid(head)
  val empty = head === tail && !valid(head)

  val s_idle :: s_req :: s_resp :: s_finish :: Nil = Enum(4)
  val state = RegInit(VecInit(Seq.fill(streamSize)(s_idle)))
  val isPrefetching = VecInit(state.map(_ =/= s_idle)).asUInt.orR

  // dequeue
  val deqLater = RegInit(false.B)
  val hitIdx = Mux(io.update.valid, io.update.bits.hitIdx, RegEnable(io.update.bits.hitIdx, io.update.valid))
  when ((io.update.valid || deqLater) && !empty && valid(hitIdx) && !isPrefetching) {
    val headBeforeHitIdx = head <= hitIdx && (hitIdx < tail || tail <= head)
    val hitIdxBeforeHead = hitIdx < tail && tail <= head
    when (headBeforeHitIdx) {
      (0 until streamSize).foreach(i => valid(i) := Mux(i.U >= head && i.U <= hitIdx, false.B, valid(i)))
    }

    when (hitIdxBeforeHead) {
      (0 until streamSize).foreach(i => valid(i) := Mux(i.U >= head || i.U <= hitIdx, false.B, valid(i)))
    }

    when (headBeforeHitIdx || hitIdxBeforeHead) {
      head := hitIdx + 1.U
      baseReq.valid := true.B
      baseReq.bits := buf(hitIdx).req
      valid(hitIdx) := false.B
      deqLater := false.B
    }
  }.elsewhen (io.update.valid && !empty && isPrefetching) {
    deqLater := true.B
  }

  val needRealloc = RegInit(false.B)

  // enqueue: send prefetch request as long as stream buffer is not full
  val tailReq = Mux(empty, baseReq.bits, buf(tail - 1.U).req)
  val prefetchReq = Wire(Vec(streamSize, Decoupled(new PrefetchReq)))
  val prefetchResp = Wire(Vec(streamSize, Decoupled(new PrefetchResp)))
  val prefetchFinish = Wire(Vec(streamSize, Decoupled(new PrefetchFinish)))
  (0 until streamSize).foreach { i =>
    prefetchReq(i).valid := state(i) === s_req
    prefetchReq(i).bits.cmd := M_XRD
    prefetchReq(i).bits.addr := get_block_addr(Mux(i.U >= tail,
      tailReq.addr + (i.U - tail + 1.U)*(CacheLineSize/8).U,
      tailReq.addr + (i.U + streamSize.U - tail + 1.U)*(CacheLineSize/8).U)) // TODO: rewrite this
    prefetchReq(i).bits.id := Cat(prefetcherID.U(clientIdWidth.W), io.entryId, i.U(log2Up(streamSize).W))

    prefetchResp(i).ready := state(i) === s_resp

    prefetchFinish(i).valid := state(i) === s_finish && !valid(i)
    prefetchFinish(i).bits.id := buf(i).resp.id

    when (!full && state(i) === s_idle && baseReq.valid && !needRealloc && !valid(i)) {
      state(i) := s_req
      buf(i).req := prefetchReq(i).bits
    }

    when (state(i) === s_req) {
      when (prefetchReq(i).fire()) {
        state(i) := s_resp
      }
    }

    when (state(i) === s_resp) {
      when (prefetchResp(i).fire()) {
        state(i) := s_finish
        buf(i).resp := prefetchResp(i).bits
      }
    }

    when (state(i) === s_finish) {
      when (prefetchFinish(i).fire() && tail === i.U && !valid(i)) {
        state(i) := s_idle
        valid(i) := true.B
        tail := tail + 1.U
      }.elsewhen (prefetchFinish(i).fire() /* && tail =/= i.U */) {
        valid(i) := true.B
      }.elsewhen (valid(i) && tail === i.U) {
        state(i) := s_idle
        tail := tail + 1.U
      }
    }
  }

  // TODO: optimize this
  val reqPrior = Wire(Vec(streamSize, Decoupled(new PrefetchReq)))
  val finishPrior = Wire(Vec(streamSize, Decoupled(new PrefetchFinish)))
  val reqArb = Module(new Arbiter(new PrefetchReq, streamSize))
  val finishArb = Module(new Arbiter(new PrefetchFinish, streamSize))
  for (i <- 0 until streamSize) {
    reqPrior(i).valid := prefetchReq(tail + i.U).valid
    reqPrior(i).bits := prefetchReq(tail + i.U).bits

    finishPrior(i).valid := prefetchFinish(tail + i.U).valid
    finishPrior(i).bits := prefetchFinish(tail + i.U).bits

    prefetchReq(i).ready := reqPrior(i.U - tail).ready
    prefetchFinish(i).ready := finishPrior(i.U - tail).ready

    reqPrior(i) <> reqArb.io.in(i)
    finishPrior(i) <> finishArb.io.in(i)
  }
  // reqArb.io.in <> reqPrior
  reqArb.io.out <> io.prefetch.req
  // finishArb.io.in <> finishPrior
  finishArb.io.out <> io.prefetch.finish

  for (i <- 0 until streamSize) {
    prefetchResp(i).bits := io.prefetch.resp.bits
    prefetchResp(i).valid := io.prefetch.resp.valid && io.prefetch.resp.bits.id(log2Up(streamSize) - 1, 0) === i.U
  }
  io.prefetch.resp.ready := VecInit(prefetchResp.zipWithIndex.map{ case (r, i) => 
    r.ready && i.U === io.prefetch.resp.bits.id(log2Up(streamSize) - 1, 0) }).asUInt.orR


  // initialize: empty buffer when state === s_idle
  val reallocReq = RegInit(0.U.asTypeOf(new StreamBufferAllocBundle))
  when ((io.alloc.valid || needRealloc) && !isPrefetching) {
    valid.foreach(_ := false.B)
    head := 0.U
    tail := 0.U
    baseReq.valid := true.B
    baseReq.bits := Mux(io.alloc.valid, io.alloc.bits, reallocReq)
    needRealloc := false.B
  }.elsewhen (io.alloc.valid && isPrefetching) {
    needRealloc := true.B
    reallocReq := io.alloc.bits
  }

  for (i <- 0 until streamSize) {
    io.addrs(i).valid := baseReq.valid && (valid(i) || state(i) =/= s_idle)
    io.addrs(i).bits := get_block_addr(buf(i).req.addr)
  }


  // debug
  XSDebug(VecInit(io.addrs.map(_.valid)).asUInt.orR, "addrs: ")
  for (i <- 0 until streamSize) {
    XSDebug(false, VecInit(io.addrs.map(_.valid)).asUInt.orR, "v:%d 0x%x  ", io.addrs(i).valid, io.addrs(i).bits)
  }
  XSDebug(false, VecInit(io.addrs.map(_.valid)).asUInt.orR, "\n")

  XSDebug(io.update.valid, p"update: ${io.update.bits}\n")
  XSDebug(io.alloc.valid, p"alloc: ${io.alloc.bits}\n")
  XSDebug(p"io.prefetch.req(${io.prefetch.req.valid} ${io.prefetch.req.ready}) ${io.prefetch.req.bits}\n")
  XSDebug(p"io.prefetch.resp(${io.prefetch.resp.valid} ${io.prefetch.resp.ready}) ${io.prefetch.resp.bits}\n")
  XSDebug(p"io.prefetch.finish(${io.prefetch.finish.valid} ${io.prefetch.finish.ready}) ${io.prefetch.finish.bits}\n")

  XSDebug("buf:\n")
  for (i <- 0 until streamSize) {
    if (i % 4 == 0) { XSDebug("") }
    XSDebug(false, true.B, p"${Hexadecimal(i.U)}: v ${valid(i)} s ${state(i)} ${buf(i)}  ")
    if (i % 4 == 3) { XSDebug(false, true.B, "\n") }
  }
  XSDebug("head=%d tail=%d full=%d empty=%d\n", head, tail, full, empty)
  XSDebug(baseReq.valid, p"baseReq: ${baseReq.bits}\n")
  XSDebug(needRealloc, p"reallocReq: ${reallocReq}\n")
  
}

class StreamPrefetcher extends PrefetcherModule {
  val io = IO(new PrefetcherIO)

  val streamBufs = Seq.fill(streamCnt) { Module(new StreamBuffer) }
  val addrValids = Wire(Vec(streamCnt, Vec(streamSize, Bool())))
  for (i <- 0 until streamCnt) {
    for (j <- 0 until streamSize) {
      addrValids(i)(j) := streamBufs(i).io.addrs(j).valid
    }
  }
  val bufValids = WireInit(VecInit(addrValids.map(_.asUInt.orR)))
  val ages = Seq.fill(streamCnt)(RegInit(0.U(ageWidth.W)))
  val maxAge = -1.S(ageWidth.W).asUInt

  // a buffer to record whether we find a stream of 2 adjacent cache lines
  def beforeEnterBufEntry = new Bundle {
    val conf = UInt(2.W)
    val addr = UInt(PAddrBits.W)

    override def toPrintable: Printable = {
      p"${conf} 0x${Hexadecimal(addr)}"
    }
  }
  val beforeEnterBuf = RegInit(VecInit(Seq.fill(streamCnt * 2)(0.U.asTypeOf(beforeEnterBufEntry))))

  // assign default values
  for (i <- 0 until streamCnt) {
    streamBufs(i).io.entryId := i.U
    streamBufs(i).io.update.valid := false.B
    streamBufs(i).io.update.bits := DontCare
    streamBufs(i).io.alloc.valid := false.B
    streamBufs(i).io.alloc.bits := DontCare
  }

  // 1. streamBufs hit
  val hit = WireInit(false.B)
  for (i <- 0 until streamCnt) {
    for (j <- 0 until streamSize) {
      when (io.in.valid && !io.in.bits.miss && addrValids(i)(j)) {
        when (streamBufs(i).io.addrs(j).bits === get_block_addr(io.in.bits.req.addr)) {
          hit := true.B
          streamBufs(i).io.update.valid := true.B
          streamBufs(i).io.update.bits.cmd := io.in.bits.req.cmd
          streamBufs(i).io.update.bits.addr := io.in.bits.req.addr
          streamBufs(i).io.update.bits.id := io.in.bits.req.id
          streamBufs(i).io.update.bits.hitIdx := j.U
          ages(i) := maxAge
        }
      }
    }
  }


  // 2. streamBufs miss
  // if this req has no adjacent cache line in beforeEnterBuf, set up a new entry and wait for next line;
  // otherwise, clear conf in beforeEnterBufEntry and set up a new stream buffer.

  // val reqLatch = RegNext(io.req)
  val inLatch = RegNext(io.in)
  val conf0Vec = VecInit(beforeEnterBuf.map(_.conf === 0.U))
  val conf1Vec = VecInit(beforeEnterBuf.map(_.conf === 1.U))
  val addrVec = VecInit(beforeEnterBuf.map(_.addr))
  val addrHits = VecInit(addrVec.map(_ === get_block_addr(inLatch.bits.req.addr))).asUInt
  val allocNewStream = WireInit(false.B)

  when (io.in.valid && io.in.bits.miss && !hit) {
    (0 until streamCnt).foreach(i => ages(i) := Mux(ages(i) =/= 0.U, ages(i) - 1.U, 0.U))
  }

  when (inLatch.valid && inLatch.bits.miss && !RegNext(hit)) {
    when ((conf1Vec.asUInt & addrHits).orR) {
      allocNewStream := true.B
      // clear off conf
      (0 until (streamCnt * 2)).foreach(i => beforeEnterBuf(i).conf := ~Fill(2, (conf1Vec(i) && addrHits(i)).asUInt) & beforeEnterBuf(i).conf)
    }.otherwise { // no adjacent line, set up a new entry in beforeEnterBuf
      val idx = Wire(UInt(log2Up(streamCnt*2).W))
      when (conf0Vec.asUInt.orR) {
        idx := PriorityMux(conf0Vec.asUInt, VecInit(List.tabulate(streamCnt*2)(_.U)))
      }.otherwise {
        idx := LFSR64()(log2Up(streamCnt*2) - 1, 0)
      }

      beforeEnterBuf(idx).conf := 1.U
      beforeEnterBuf(idx).addr := get_block_addr(inLatch.bits.req.addr) + (1.U << blockOffBits)
    }
  }

  when (allocNewStream) {
    val idx = Wire(UInt(log2Up(streamCnt).W))

    // refill an invalid or the eldest stream buffer with new one
    when ((~bufValids.asUInt).orR) {
      idx := PriorityMux(~bufValids.asUInt, VecInit(List.tabulate(streamCnt)(_.U)))
    }.otherwise {
      val ageSeq = Seq.fill(streamCnt)(Wire(new CompareBundle))
      for (i <- 0 until streamCnt) {
        ageSeq(i).bits := ages(i)
        ageSeq(i).idx := i.U
      }
      // idx := ParallelMin(ages.zipWithIndex.map{case (a,b) => (a, b.U)})._2
      idx := ParallelMin(ageSeq).idx
    }

    for (i <- 0 until streamCnt) {
      streamBufs(i).io.alloc.valid := idx === i.U
      streamBufs(i).io.alloc.bits := inLatch.bits.req
      streamBufs(i).io.alloc.bits.addr := get_block_addr(inLatch.bits.req.addr)
      when (idx === i.U) { ages(i) := maxAge }
    }
  }

  // 3. send prefetch req from stream buffer
  val reqArb = Module(new Arbiter(new PrefetchReq, streamCnt))
  val finishArb = Module(new Arbiter(new PrefetchFinish, streamCnt))
  for (i <- 0 until streamCnt) {
    reqArb.io.in(i).valid := streamBufs(i).io.prefetch.req.valid
    reqArb.io.in(i).bits := streamBufs(i).io.prefetch.req.bits
    streamBufs(i).io.prefetch.req.ready := reqArb.io.in(i).ready

    streamBufs(i).io.prefetch.resp.valid := io.prefetch.resp.valid && (io.prefetch.resp.bits.id(entryIdMSB, entryIdLSB) >> log2Up(streamSize)) === i.U
    streamBufs(i).io.prefetch.resp.bits := io.prefetch.resp.bits

    finishArb.io.in(i).valid := streamBufs(i).io.prefetch.finish.valid
    finishArb.io.in(i).bits := streamBufs(i).io.prefetch.finish.bits
    streamBufs(i).io.prefetch.finish.ready := finishArb.io.in(i).ready
  }

  io.prefetch.resp.ready := VecInit(streamBufs.zipWithIndex.map{ case (buf, i) => 
    i.U === (io.prefetch.resp.bits.id(entryIdMSB, entryIdLSB) >> log2Up(streamSize)) && buf.io.prefetch.resp.ready }).asUInt.orR

  io.prefetch.req <> reqArb.io.out

  io.prefetch.finish <> finishArb.io.out
  
  // debug
  XSDebug("clientIdWidth=%d entryIdWidth=%d\n", clientIdWidth.U, entryIdWidth.U)
  XSDebug(io.in.valid, p"in: ${io.in.bits.req} miss=${io.in.bits.miss}  streanBufs hit=${hit}\n")
  XSDebug(p"io.prefetch.req(${io.prefetch.req.valid} ${io.prefetch.req.ready}) ${io.prefetch.req.bits}\n")
  XSDebug(p"io.prefetch.resp(${io.prefetch.resp.valid} ${io.prefetch.resp.ready}) ${io.prefetch.resp.bits}\n")
  XSDebug(p"io.prefetch.finish(${io.prefetch.finish.valid} ${io.prefetch.finish.ready}) ${io.prefetch.finish.bits}\n")
  
  XSDebug("")
  for (i <- 0 until streamCnt) {
    XSDebug(false, true.B, "%d: age=%d  ", i.U, ages(i))
  }
  XSDebug(false, true.B, "\n")

  XSDebug("beforeEnterBuf:\n")
  for (i <- 0 until (streamCnt*2)) {
    if (i % 4 == 0) { XSDebug("") }
    XSDebug(false, true.B, p"${beforeEnterBuf(i)} ")
    if (i % 4 == 3) { XSDebug(false, true.B, "\n") }
  }
  XSDebug("conf0Vec=%b conf1Vec=%b addrHits=%b\n", conf0Vec.asUInt, conf1Vec.asUInt, addrHits)

  XSDebug(inLatch.valid, p"inLatch: ${inLatch.bits.req} miss=${inLatch.bits.miss}  allocNewStream=${allocNewStream}\n")

}