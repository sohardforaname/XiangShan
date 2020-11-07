package xiangshan.cache.prefetcher

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientMetadata
import xiangshan._
import utils._
import scala.math.max

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
  io.prefetch.finish.bits.id := resp.id

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