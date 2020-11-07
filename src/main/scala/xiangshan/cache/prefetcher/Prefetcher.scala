package xiangshan.cache.prefetcher

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientMetadata
import xiangshan._
import xiangshan.cache._
import utils._
import scala.math.max


trait HasPrefetcherParameters extends HasXSParameter with MemoryOpConstants {
  val pcfg = l1plusPrefetcherParameters

  val blockOffBits = log2Up(icacheParameters.blockBytes)
  def get_block_addr(addr: UInt) = (addr >> blockOffBits) << blockOffBits

  def streamCnt = pcfg.streamCnt
  def streamSize = pcfg.streamSize
  def ageWidth = pcfg.ageWidth
  
  def icachemisQueueEntryIdWidth = log2Up(icacheParameters.nMissEntries)
  def prefetcherEntryIdWidth = log2Up(pcfg.streamCnt*pcfg.streamSize)
  def clientIdWidth = log2Up(l1plusCacheParameters.nClients) // l1i miss queue and l1+ prefetcher
  def entryIdWidth = max(icachemisQueueEntryIdWidth, prefetcherEntryIdWidth)
  def idWidth = clientIdWidth + entryIdWidth
  def entryIdMSB = entryIdWidth - 1
  def entryIdLSB = 0

  def prefetcherID = pcfg.id
}

abstract class PrefetcherModule extends XSModule
  with HasPrefetcherParameters

abstract class PrefetcherBundle extends XSBundle
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

  io.prefetch.req.valid := prefetcher.io.prefetch.req.valid
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
