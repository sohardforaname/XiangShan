package xiangshan.cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientMetadata
import xiangshan._
import utils._

trait HasPrefetcherParameters extends HasL1plusCacheParameters {
  def clientIdWidth = icachemisQueueClientIdWidth
  def entryIdWidth = icachemisQueueEntryIdWidth
  def idWidth = clientIdWidth + entryIdWidth
  def clientIdMSB = idWidth - 1
  def clientIdLSB = entryIdWidth
  def entryIdMSB = entryIdWidth - 1
  def entryIdLSB = 0
  // val streamCnt = 4
  val streamSize = 4
  val ageWidth = 4
}

abstract class PrefetcherModule extends L1CacheModule
  // with HasDCacheParameters
  with HasPrefetcherParameters

abstract class PrefetcherBundle extends L1CacheBundle
  // with HasDCacheParameters
  with HasPrefetcherParameters
  
class PrefetchReq extends PrefetcherBundle {
  val cmd = UInt(M_SZ.W)
  val addr = UInt(PAddrBits.W)
  val id = UInt(idWidth.W)

  override def toPrintable: Printable = {
    p"cmd=${Hexadecimal(cmd)} addr=0x${Hexadecimal(addr)} id=${id}"
  }
}

class PrefetchResp extends PrefetcherBundle {
  val id = UInt(idWidth.W)

  override def toPrintable: Printable = {
    p"id=${id}"
  }
}

class PrefetchFinish extends PrefetcherBundle {
  val id = UInt(idWidth.W)

  override def toPrintable: Printable = {
    p"id=${id}"
  }
}

class PrefetcherIO extends PrefetcherBundle {
  val in = Flipped(ValidIO(new Bundle {
    val req = new PrefetchReq
    val miss = Bool()
  }))

  val prefetch = new Bundle {
    val req = DecoupledIO(new PrefetchReq)
    val resp = Flipped(DecoupledIO(new PrefetchResp))
    val finish = DecoupledIO(new PrefetchFinish)
  }
}

class FakePrefetcher extends PrefetcherModule {
  val io = IO(new PrefetcherIO)

  io.prefetch.req.valid := false.B
  io.prefetch.req.bits := DontCare
  io.prefetch.resp.ready := true.B
  io.prefetch.finish.valid := false.B
  io.prefetch.finish.bits := DontCare

  assert(!io.prefetch.resp.fire(), "Fake prefetcher should not receive resp!")
}

class NextLinePrefetcher extends PrefetcherModule {
  val io = IO(new PrefetcherIO)

  val s_idle :: s_req :: s_resp :: s_finish :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val req = Reg(new PrefetchReq)
  val resp = Reg(new PrefetchResp)

  io.prefetch.req.valid := state === s_req
  // io.prefetch.req.bits.cmd := Mux(isRead(req.cmd), M_PFR, M_PFW)
  io.prefetch.req.bits.cmd := M_XRD
  io.prefetch.req.bits.addr := req.addr + (CacheLineSize / 8).U
  io.prefetch.req.bits.id := DontCare

  io.prefetch.resp.ready := state === s_resp

  io.prefetch.finish.valid := state === s_finish
  io.prefetch.finish.bits.client_id := resp.client_id
  io.prefetch.finish.bits.entry_id := resp.entry_id

  when (state === s_idle) {
    when (io.in.valid) {
      state := s_req
      req := io.in.bits.req
    }
  }

  when (state === s_req) {
    when (io.prefetch.req.fire()) {
      state := s_resp
    }
  }

  when (state === s_resp) {
    when (io.prefetch.resp.fire()) {
      state := s_finish
      resp := io.prefetch.resp.bits
    }
  }

  when (state === s_finish) {
    when (io.prefetch.finish.fire()) {
      state := s_idle
    }
  }
}

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
    // val entryId = Input(UInt(clientMissQueueEntryIdWidth.W))
    val entryId = Input(UInt(entryIdWidth.W))
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
  val state = RegInit(s_idle)

  // dequeue
  when (io.update.valid) {
    val hitIdx = io.update.bits.hitIdx
    when (!empty && valid(hitIdx)) {

      // hitIdx between head and tail
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
      }
    }
  }


  // enqueue: send prefetch request as long as stream buffer is not full
  val tailReq = Mux(empty, baseReq.bits, buf(tail - 1.U).req)
  val prefetchReq = WireInit(tailReq)
  prefetchReq.cmd := M_XRD
  prefetchReq.addr := tailReq.addr + (CacheLineSize / 8).U
  prefetchReq.id := Cat(0.U(clientIdWidth.W), io.entryId)

  when (!full && state === s_idle && baseReq.valid) {
    state := s_req
    buf(tail).req := prefetchReq
  }

  when (state === s_req) {
    when (io.prefetchReq.fire()) {
      state := s_resp
    }
  }

  when (state === s_resp) {
    when (io.prefetchResp.fire()) {
      state := s_finish
      buf(tail).resp := io.prefetchResp.bits
    }
  }

  when (state === s_finish) {
    when (io.prefetchFinish.fire()) {
      state := s_idle
      valid(tail) := true.B
      tail := tail + 1.U
    }
  }


  // initialize: empty buffer when state === s_idle
  val needRealloc = RegInit(false.B)
  val reallocReq = RegInit(0.U.asTypeOf(new StreamBufferAllocBundle))
  when ((io.alloc.valid || needRealloc) && state === s_idle) {
    valid.foreach(_ := false.B)
    head := 0.U
    tail := 0.U
    baseReq.valid := true.B
    baseReq.bits := Mux(io.alloc.valid, io.alloc.bits, reallocReq)
    needRealloc := false.B
  }.elsewhen (io.alloc.valid && state =/= s_idle) {
    needRealloc := true.B
    reallocReq := io.alloc.bits
  }

  for (i <- 0 until streamSize) {
    io.addrs(i).valid := baseReq.valid && valid(i)
    io.addrs(i).bits := get_block_addr(buf(i).req.addr)
  }
  io.prefetchr.req.valid := state === s_req
  io.prefetch.req.bits := prefetchReq
  io.prefetch.req.bits.addr := get_block_addr(prefetchReq.addr)
  io.prefetch.resp.ready := state === s_resp
  io.prefetch.finish.valid := state === s_finish
  // io.prefetch.finish.bits.client_id := buf(tail).resp.client_id
  // io.prefetch.finish.bits.entry_id := buf(tail).resp.entry_id
  io.prefetch.finish.bits.id := buf(tail).resp.id


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
    XSDebug(false, true.B, p"${Hexadecimal(i.U)} v ${valid(i)} ${buf(i)}  ")
    if (i % 4 == 3) { XSDebug(false, true.B, "\n") }
  }
  XSDebug("state=%d head=%d tail=%d full=%d empty=%d\n", state, head, tail, full, empty)
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

    streamBufs(i).io.prefetch.resp.valid := io.prefetch.resp.valid && io.prefetch.resp.bits.id(entryIdMSB, entryIdLSB) === i.U
    streamBufs(i).io.prefetch.resp.bits := io.prefetch.resp.bits

    finishArb.io.in(i).valid := streamBufs(i).io.prefetch.finish.valid
    finishArb.io.in(i).bits := streamBufs(i).io.prefetch.finish.bits
    streamBufs(i).io.prefetch.finish.ready := finishArb.io.in(i).ready
  }

  io.prefetch.resp.ready := streamBufs.zipWithIndex.map((buf, i) => 
    i.U === io.prefetch.resp.bits.id(entryIdMSB, entryIdLSB) && buf.io.prefetch.resp.ready).asUInt.orR

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


class L1IplusPrefetcher(enable: Boolean) extends PrefetcherModule {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new Bundle {
      val req = new IcacheMissReq
      val miss = Bool()
    }))
    val prefetch = new Bundle {
      val req = DecoupledIO(new L1plusCacheReq)
      val resp = Flipped(DecoupledIO(new L1plusCacheResp))
    }
  })

  val prefetcher = if (enable) Module(new StreamPrefetcher) else Module(new FakePrefetcher)

  io.in.ready := true.B
  prefetcher.io.in.valid := io.in.fire()
  prefetcher.io.in.bits := DontCare
  prefetcher.io.in.bits.req.cmd := M_XRD
  prefetcher.io.in.bits.req.addr := io.in.bits.req.addr
  prefetcher.io.in.bits.miss := io.in.bits.miss

  io.prefetch.req.valid := prefetcher.io.prefetch_req.valid
  io.prefetch.req.bits := DontCare
  io.prefetch.req.bits.cmd := M_XRD // or M_PFR
  io.prefetch.req.bits.addr := prefetcher.io.prefetch.req.bits.addr
  io.prefetch.req.bits.id := prefetcher.io.prefetch.req.bits.id
  prefetcher.io.prefetch.req.ready := io.prefetch.req.ready

  prefetcher.io.prefetch.resp.valid := io.prefetch.resp.valid
  prefetcher.io.prefetch.resp.bits.id := io.prefetch.resp.bits.id
  io.prefetch.resp.ready := prefetcher.io.prefetch.resp.ready

  prefetcher.io.prefetch.finish.ready := true.B
}
