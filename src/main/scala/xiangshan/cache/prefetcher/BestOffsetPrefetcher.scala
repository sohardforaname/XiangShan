package xiangshan.cache.prefetcher

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientMetadata
import xiangshan._
import xiangshan.cache._
import utils._

trait BestOffsetPrefetcherParameters {
  def rrTableEntries: Int // recent requests table
  def rrTagBits: Int
  def scoreBits: Int
  def roundMax: Int
  def badScore: Int // threshold
  def scores: Int
  def offsetList: Seq[Int]
}

case class L2PrefetcherParameters (
  rrTableEntries: Int = 256,
  rrTagBits: Int = 12,
  scoreBits: Int = 5,
  roundMax: Int = 100,
  badScore: Int = 1,
  offsetList: Seq[Int] = Seq(
      1,   2,   3,   4,   5,   6,   8,   9,  10,  12,
     15,  16,  18,  20,  24,  25,  27,  30,  32,  36,
     40,  45,  48,  50,  54,  60,  64,  72,  75,  80,
     81,  90,  96, 100, 108, 120, 125, 128, 135, 144,
    150, 160, 162, 180, 192, 200, 216, 225, 240, 243,
    250, 256
  ),
  scores: Int = 52
) extends BestOffsetPrefetcherParameters

class ScoreTableEntry extends PrefetcherBundle {
  val offset = UInt(offsetWidth.W)
  val score = UInt(scoreBits.W)
}

object ScoreTableEntry {
  def apply(offset: UInt, score: UInt) = {
    val e = new ScoreTableEntry
    e.offset := offset
    e.score := score
    e
  }
}

class TestOffsetReq extends PrefetcherBundle {
  // We need to find whether (X-d) is in recent request table.
  val addr = UInt(PAddrBits.W) // X
  val testOffset = UInt(offsetWidth.W) // d
  val ptr = UInt(log2Up(scores).W) // index of testOffset in offset list
}

class TestOffsetResp extends PrefetcherBundle {
  val hit = Bool()
  val testOffset = UInt(offsetWidth.W)
  val ptr = UInt(log2Up(scores).W)
}

class TestOffsetBundle extends PrefetcherBundle {
  val req = DecoupledIO(new TestOffsetReq)
  val resp = Flipped(ValidIO(new TestOffsetResp))
}

class RecentRequestTable extends PrefetcherModule {
  val io = IO(new Bundle {
    val write = Flipped(ValidIO(UInt(PAddrBits.W)))
    val read = Flipped(new TestOffsetBundle)
  })

  // RR table is direct mapped, accessed through a hash function, each entry holding a partial tag.
  //        +----------+---------------+---------------+----------------------+
  // paddr: |  ......  |  8-bit hash2  |  8-bit hash1  |  6-bit cache offset  |
  //        +----------+---------------+---------------+----------------------+
  //        +-------+------------------+---------------+----------------------+
  //    or: |  ...  |    12-bit tag    |  8-bit hash1  |  6-bit cache offset  |
  //        +-------+------------------+---------------+----------------------+
  def lineAddr(paddr: UInt) = paddr(PAddrBits - 1, log2Up(CacheLineSize/8))
  def hash1(paddr: UInt) = lineAddr(paddr)(rrIdxBits - 1, 0)
  def hash2(paddr: UInt) = lineAddr(paddr)(rrIdxBits*2 - 1, rrIdxBits)
  def idx(paddr: UInt) = hash1(paddr) ^ hash2(paddr)
  def tag(paddr: UInt) = lineAddr(paddr)(rrTagBits + rrIdxBits - 1, rrIdxBits)
  def rrTableEntry() = new Bundle {
    val valid = Bool()
    val tag = UInt(rrTagBits.W)
  }

  val rrTable = Module(new SRAMTemplate(rrTableEntry(), set = rrTableEntries, way = 1, shouldReset = true))

  val wAddr = io.write.bits
  rrTable.io.w.req.valid := io.write.valid
  rrTable.io.w.req.bits.setIdx := idx(wAddr)
  rrTable.io.w.req.bits.data.valid := true.B
  rrTable.io.w.req.bits.data.tag := tag(wAddr)

  val rAddr = io.read.req.bits.addr - (io.read.req.bits.testOffset << log2Up(CacheLineSize/8))
  val rData = Wire(rrTableEntry())
  rrTable.io.r.req.valid := io.read.req.fire()
  rrTable.io.r.req.bits.setIdx := idx(rAddr)
  rData := rrTable.io.r.resp.data(0)

  val wrConflict = io.write.valid && io.read.req.fire() && idx(wAddr) === idx(rAddr)
  when (wrConflict) {
    rrTable.io.r.req.valid := false.B
  }
  when (RegNext(wrConflict)) {
    rData.valid := true.B
    rData.tag := RegNext(tag(wAddr))
  }

  io.read.req.ready := true.B
  io.read.resp.valid := RegNext(io.read.req.fire())
  io.read.resp.bits.testOffset := RegNext(io.read.req.bits.testOffset)
  io.read.resp.bits.ptr := RegNext(io.read.req.bits.ptr)
  io.read.resp.bits.hit := rData.valid && rData.tag === RegNext(tag(rAddr))

}

class OffsetScoreTable extends PrefetcherModule {
  val io = IO(new Bundle {
    val prefetchOffset = Output(UInt(offsetWidth.W))
    val test = new TestOffsetBundle
  })

  val prefetchOffset = RegInit(1.U(offsetWidth.W)) // best offset is 1, that is, a next-line prefetcher as initialization
  val st = RegInit(VecInit(offsetList.map(offset => ScoreTableEntry(offset.U, 0.U))))
  val ptr = RegInit(0.U(log2Up(scores).W)) // 6 bit
  val round = RegInit(0.U(roundBits.W)) // 7 bit
  val firstInit = RegInit(true.B)
  val testOffset = WireInit(0.U(offsetWidth.W))

  val winnerEntry = RegInit(ScoreTableEntry(1.U, 0.U))
  def winner(e1: ScoreTableEntry, e2: ScoreTableEntry): ScoreTableEntry = {
    val w = new ScoreTableEntry
    w := Mux(e1.score > e2.score, e1, e2)
    w
  }

  val s_init :: s_learn :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_init)

  // 1. At the start of a learning phase
  // All the scores are reset to 0.
  when (state === s_init) {
    when (firstInit) {
      firstInit := false.B
      state := s_learn
      ptr := 0.U
    }.otherwise {
      when (ptr =/= scores.U) {
        st(ptr).score := 0.U
        ptr := ptr + 1.U
      }.otherwise {
        ptr := 0.U
        state := s_learn
      }
    }
  }


  // 2. During a learning phase
  // On every eligible L2 read access (miss or prefetched hit), we test an offset d_i from the list.
  // If X-d_i hits in the RR table, the score of offset d_i is incremented. During a round, each offset
  // in the list is test once. When all the offsets in the list have been tested, the current round is
  // finished, and a new round begins from offset d_1 again.
  // The current learning phase finishes at the end of a round when:
  // (1) one of the score equals SCOREMAX, or
  // (2) the number of rounds equals ROUNDMAX.
  when (state === s_learn) {

    testOffset := st(ptr).offset
    when (io.test.req.fire()) {
      val roundFinish = ptr === (scores - 1).U
      ptr := Mux(roundFinish, 0.U, ptr + 1.U)
      round := Mux(roundFinish, round + 1.U, round)
    }

    when (round === roundMax.U) {
      state := s_finish
    }

    when (io.test.resp.valid && io.test.resp.bits.hit) {
      val oldScore = st(io.test.resp.bits.ptr).score
      val newScore = oldScore + 1.U
      val offset = st(io.test.resp.bits.ptr).offset
      st(io.test.resp.bits.ptr).score := newScore
      winnerEntry := winner(ScoreTableEntry(offset, newScore), winnerEntry)
      // (1) one of the score equals SCOREMAX
      when (newScore === scoreMax.U) {
        state := s_finish
      }
    }

  }


  // 3. At the end of every learning phase, the prefetch offset is updated as the one with the highest score.
  when (state === s_finish) {
    prefetchOffset := winnerEntry.offset
    ptr := 0.U
    round := 0.U
    winnerEntry.offset := 1.U
    winnerEntry.score := 0.U
    state := s_init
  }


  io.prefetchOffset := prefetchOffset
  io.test.req.valid := state === s_learn && round =/= roundMax.U
  io.test.req.bits.addr := DontCare
  io.test.req.bits.testOffset := testOffset
  io.test.req.bits.ptr := ptr

}

class BestOffsetPrefetcherIO extends PrefetcherBundle {
  val in = Flipped(DecoupledIO(new Bundle {
    val req = new PrefetchReq
    val miss = Bool() // Is this request miss?
    val prefetched = Bool() // If hit, is this a prefetched hit?
  }))

  val prefetch = new Bundle {
    val req = DecoupledIO(new PrefetchReq)
    val resp = Flipped(DecoupledIO(new PrefetchResp))
    val finish = DecoupledIO(new PrefetchFinish)
  }
}

class BestOffsetPrefetcherEntryIO extends BestOffsetPrefetcherIO {
  val prefetchOffset = Input(UInt(offsetWidth.W))
  val id = Input(UInt(log2Up(prefetchEntries).W))
  val addr = Output(ValidIO(UInt(PAddrBits.W)))
}

class BestOffsetPrefetcherEntry extends PrefetcherModule {
  val io = IO(new BestOffsetPrefetcherEntryIO)

  val s_idle :: s_req :: s_resp :: s_finish :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val req = Reg(new PrefetchReq)
  val resp = Reg(new PrefetchResp)

  when (state === s_idle) {
    when (io.in.fire()) {
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

  io.in.ready := state === s_idle
  io.prefetch.req.valid := state === s_req
  io.prefetch.req.bits.cmd := M_XRD // TODO: specify this
  io.prefetch.req.bits.addr := req.addr + (io.prefetchOffset << (log2Up(CacheLineSize/8)))
  io.prefetch.req.bits.id := DontCare // TODO
  io.prefetch.resp.ready := state === s_resp
  io.prefetch.finish.valid := state === s_finish
  io.prefetch.finish.bits.id := resp.id
  io.addr.valid := state =/= s_idle
  io.addr.bits := req.addr

}

class BestOffsetPrefetcher extends PrefetcherModule {
  val io = IO(new BestOffsetPrefetcherIO)

  val rrTable = Module(new RecentRequestTable)
  val scoreTable = Module(new OffsetScoreTable)

  val prefetchOffset = scoreTable.io.prefetchOffset

  // 1. Prefetch
  // When a read request for line X accesses the L2 cache, if this is a miss or a prefetched hit 
  // (i.e., the prefetch bit is set), and if X and X+D lie in the same memory page, a prefetch 
  // for line X+D is sent to the L3 cache.
  val entryFree = Wire(Vec(prefetchEntries, Bool()))
  val entryAllocIdx = Wire(UInt())

  val reqArb = Module(new Arbiter(new PrefetchReq, prefetchEntries))
  val finishArb = Module(new Arbiter(new PrefetchFinish, prefetchEntries))

  val entries = (0 until prefetchEntries).map { i => 
    val e = Module(new BestOffsetPrefetcherEntry)

    e.io.id := i.U(log2Up(prefetchEntries).W)
    e.io.prefetchOffset := prefetchOffset

    e.io.in.valid := io.in.valid && (io.in.bits.miss || io.in.bits.prefetched) && entryFree.asUInt.orR && entryAllocIdx === i.U
    e.io.in.bits := io.in.bits
    io.in.ready := true.B

    reqArb.io.in(i) <> e.io.prefetch.req

    e.io.prefetch.resp.valid := io.prefetch.resp.valid && io.prefetch.resp.bits.id === i.U  // TODO
    e.io.prefetch.resp.bits := io.prefetch.resp.bits

    finishArb.io.in(i) <> e.io.prefetch.finish
    e
  }
  entryFree := VecInit(entries.map(e => e.io.in.ready))
  entryAllocIdx := PriorityEncoder(entryFree)

  io.prefetch.req <> reqArb.io.out

  io.prefetch.resp.ready := VecInit(entries.zipWithIndex.map { case (e, i) => 
    i.U === io.prefetch.resp.bits.id && e.io.prefetch.resp.ready }).asUInt.orR

  io.prefetch.finish <> finishArb.io.out
  

  // 2. Record recent requests
  // We record in a recent requests table (rrTable) the base address of prefetch requests that have
  // been completed. The base address is the address that was used to trigger the prefetch request.
  rrTable.io.write.valid := io.prefetch.finish.fire()
  rrTable.io.write.bits := PriorityMux(entries.map(e => e.io.prefetch.finish.valid) zip entries.map(e => e.io.addr.bits))


  // 3. Best offset learning
  rrTable.io.read.req.valid := scoreTable.io.test.req.valid && io.in.fire()
  rrTable.io.read.req.bits.addr := io.in.bits.req.addr
  rrTable.io.read.req.bits.testOffset := scoreTable.io.test.req.bits.testOffset
  scoreTable.io.test.req.ready := io.in.fire()

  scoreTable.io.test.resp <> rrTable.io.read.resp


}