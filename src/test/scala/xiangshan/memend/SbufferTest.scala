package xiangshan.memend

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import xiangshan._
import xiangshan.cache.{DCacheLineIO, DCacheWordReq}
import xiangshan.mem.{LoadForwardQueryIO, NewSbuffer}
import xiangshan.testutils._

import scala.util.Random

class SbufferWapper extends XSModule {
  val io = IO(new Bundle() {
    val in = Vec(StorePipelineWidth, Flipped(Decoupled(new DCacheWordReq)))
    val dcache = new DCacheLineIO
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
    val flush = new Bundle {
      val valid = Input(Bool())
      val empty = Output(Bool())
    } // sbuffer flush
  })
  val sbuffer = Module(new NewSbuffer)
  io <> sbuffer.io

  // fake dcache
  //sbuffer.io.dcache.req.ready := true.B
  sbuffer.io.dcache.resp.valid := RegNext(RegNext(RegNext(RegNext(sbuffer.io.dcache.req.valid))))
  sbuffer.io.dcache.resp.bits.meta.id := RegNext(RegNext(RegNext(RegNext(sbuffer.io.dcache.req.bits.meta.id))))
}

class SbufferTest extends AnyFlatSpec
  with ChiselScalatestTester
  with Matchers
  with HasXSParameter
  with ParallelTestExecution
  with HasPartialDecoupledDriver {


  top.Parameters.set(top.Parameters.debugParameters)

  it should "random req" in {
    test(new SbufferWapper{AddSinks()}){ c =>

      def store_enq(addr: Seq[UInt], data: Seq[UInt], mask: Seq[UInt]) ={
        (0 until StorePipelineWidth).foreach{ i =>
          c.io.in(i).valid.poke(true.B)
          c.io.in(i).bits.pokePartial(chiselTypeOf(c.io.in(i).bits).Lit(
            _.mask -> mask(i),
            _.addr -> addr(i),
            _.data -> data(i)
          ))
        }
        c.clock.step(1)
        for (in <- c.io.in){ in.valid.poke(false.B)}
      }

      def forward_req_and_resp(addr: Seq[UInt], data: Seq[UInt], mask:Seq[UInt]) = {
        (0 until LoadPipelineWidth).map{ i =>
          c.io.forward(i).paddr.poke(addr(i))
          c.io.forward(i).mask.poke(mask(i))
          if(c.io.in(i).ready.peek() == true.B) {
            (0 until 8).map { j =>
              c.io.forward(i).forwardData(j).expect(data(i)(j * 8 + 7, j * 8))
            }
          }
        }
      }

      val TEST_SIZE = 100
      for(i <- 0 until TEST_SIZE) {
        val addr = Seq.fill(StorePipelineWidth)((Random.nextLong() & 0x7ffffffff8L).U)// align to block size
        val data = Seq.fill(StorePipelineWidth)((Random.nextLong() & 0x7fffffffffffffffL).U)
        val mask = Seq.fill(StorePipelineWidth)(0xff.U)
        store_enq(addr, data, mask)
        forward_req_and_resp(addr, data, mask)
      }
    }
  }

  it should "sequence req" in {
    test(new SbufferWapper{AddSinks()}){ c =>

      def store_enq(addr: Seq[UInt], data: Seq[UInt], mask: Seq[UInt]) = {
        (0 until StorePipelineWidth).foreach { i =>
          c.io.in(i).valid.poke(true.B)
          c.io.in(i).bits.pokePartial(chiselTypeOf(c.io.in(i).bits).Lit(
            _.mask -> mask(i),
            _.addr -> addr(i),
            _.data -> data(i)
          ))
        }
        c.clock.step(1)
        for (in <- c.io.in){ in.valid.poke(false.B)}
      }

      def forward_req_and_resp(addr: Seq[UInt], data: Seq[UInt], mask:Seq[UInt]) = {
        (0 until LoadPipelineWidth).map{ i =>
          c.io.forward(i).paddr.poke(addr(i))
          c.io.forward(i).mask.poke(mask(i))
          if(c.io.in(i).ready.peek() == true.B) {
            (0 until 8).map { j =>
              c.io.forward(i).forwardData(j).expect(data(i)(j * 8 + 7, j * 8))
            }
          }
        }
      }

      val TEST_SIZE = 100
      val start_addr = Random.nextLong() & 0x7ffffffff8L
      for(i <- 0 until TEST_SIZE) {
        val addr = Seq(((i<<4) + start_addr).U,((i<<4)+8+start_addr).U)
        val data = Seq.fill(StorePipelineWidth)((Random.nextLong() & 0x7fffffffffffffffL).U)
        val mask = Seq.fill(StorePipelineWidth)(0xff.U)
        store_enq(addr, data, mask)
        forward_req_and_resp(addr, data, mask)
      }
    }
  }

//  //TODO: RUN THIS TEST MUST ANNOTATE LINE 31 AND TWO TEST ABOVE
//  it should "corner test" in {
//    test(new SbufferWapper{AddSinks()}){ c =>
//      def store_enq(addr: Seq[UInt], data: Seq[UInt], mask: Seq[UInt]) = {
//        (0 until StorePipelineWidth).foreach { i =>
//          c.io.in(i).valid.poke(true.B)
//          c.io.in(i).bits.pokePartial(chiselTypeOf(c.io.in(i).bits).Lit(
//            _.mask -> mask(i),
//            _.addr -> addr(i),
//            _.data -> data(i)
//          ))
//        }
//        c.clock.step(1)
//        for (in <- c.io.in){ in.valid.poke(false.B)}
//      }
//
//      def forward_req_and_resp(addr: Seq[UInt], data: Seq[UInt], mask:Seq[UInt]) = {
//        (0 until LoadPipelineWidth).map{ i =>
//          c.io.forward(i).paddr.poke(addr(i))
//          c.io.forward(i).mask.poke(mask(i))
//          if(c.io.in(i).ready.peek() == true.B) {
//            (0 until 8).map { j =>
//              c.io.forward(i).forwardData(j).expect(data(i)(j * 8 + 7, j * 8))
//            }
//          }
//        }
//      }
//
//      def store_forward_random(addr: Seq[UInt], data: Seq[UInt], mask: Seq[UInt] = Seq(0xff.U, 0xff.U)): Unit ={
//        store_enq(addr, data, mask)
//        forward_req_and_resp(addr, data, mask)
//      }
//
//      def legalRandomData = (Random.nextLong() & 0x7fffffffffffffffL).U
//
//      c.clock.step(1)
//      c.io.dcache.req.ready.poke(false.B)
//      store_forward_random(Seq(0x000.U, 0x008.U), Seq(legalRandomData, legalRandomData))
//      store_forward_random(Seq(0x040.U, 0x048.U), Seq(legalRandomData, legalRandomData))
//      store_forward_random(Seq(0x080.U, 0x088.U), Seq(legalRandomData, legalRandomData))
//      store_forward_random(Seq(0x0c0.U, 0x0c8.U), Seq(legalRandomData, legalRandomData))
//      store_forward_random(Seq(0x100.U, 0x108.U), Seq(legalRandomData, legalRandomData))
//      store_forward_random(Seq(0x140.U, 0x148.U), Seq(legalRandomData, legalRandomData))
//      store_forward_random(Seq(0x180.U, 0x188.U), Seq(legalRandomData, legalRandomData))
//      store_forward_random(Seq(0x1c0.U, 0x1c8.U), Seq(legalRandomData, legalRandomData))
//      store_forward_random(Seq(0x200.U, 0x208.U), Seq(legalRandomData, legalRandomData))
//      store_forward_random(Seq(0x240.U, 0x248.U), Seq(legalRandomData, legalRandomData))
//      store_forward_random(Seq(0x280.U, 0x288.U), Seq(legalRandomData, legalRandomData))
//      store_forward_random(Seq(0x2c0.U, 0x2c8.U), Seq(legalRandomData, legalRandomData))
//
//      // c.clock.step(1)
//      // this req can be merge
//
//      store_forward_random(Seq(0x010.U, 0x018.U), Seq(legalRandomData, legalRandomData))
//      store_forward_random(Seq(0x020.U, 0x028.U), Seq(legalRandomData, legalRandomData))
//      // this should be wrote to another line
//      c.io.dcache.req.ready.poke(true.B)
//      store_forward_random(Seq(0x030.U, 0x038.U), Seq(legalRandomData, legalRandomData))
//      c.clock.step(1)
//
//    }
//  }

//  //TODO: RUN THIS TEST MUST ANNOTATE LINE 31 AND TWO TEST ABOVE
//  it should "data connect test" in {
//    test(new SbufferWapper{AddSinks()}){ c =>
//      def store_enq(addr: Seq[UInt], data: Seq[UInt], mask: Seq[UInt], step:Boolean = true) = {
//        (0 until StorePipelineWidth).foreach { i =>
//          c.io.in(i).valid.poke(true.B)
//          c.io.in(i).bits.pokePartial(chiselTypeOf(c.io.in(i).bits).Lit(
//            _.mask -> mask(i),
//            _.addr -> addr(i),
//            _.data -> data(i)
//          ))
//        }
//        if(step){
//          c.clock.step(1)
//          for (in <- c.io.in){ in.valid.poke(false.B)}
//        }
//      }
//
//      def forward_req_and_resp(addr: Seq[UInt], data: Seq[UInt], mask:Seq[UInt]) = {
//        (0 until LoadPipelineWidth).map{ i =>
//          c.io.forward(i).paddr.poke(addr(i))
//          c.io.forward(i).mask.poke(mask(i))
//          if(c.io.in(i).ready.peek() == true.B) {
//            (0 until 8).map { j =>
//              c.io.forward(i).forwardData(j).expect(data(i)(j * 8 + 7, j * 8))
//            }
//          }
//        }
//      }
//
//      def store_forward_random(addr: Seq[UInt], data: Seq[UInt], mask: Seq[UInt] = Seq(0xff.U, 0xff.U)): Unit ={
//        store_enq(addr, data, mask)
//        forward_req_and_resp(addr, data, mask)
//      }
//
//      def legalRandomData = (Random.nextLong() & 0x7fffffffffffffffL).U
//
//
//
//      val loData = Random.nextLong() & 0x7fffffff
//      val hiData = Random.nextLong() & 0x7fffffff
//      val expectedData = (hiData << 32) + loData
//
//      c.clock.step(1)
//      c.io.dcache.req.ready.poke(false.B)
//      store_enq(Seq(0x000.U, 0x000.U), Seq(loData.U, loData.U), Seq(0x0f.U, 0x0f.U))
//      store_enq(Seq(0x040.U, 0x040.U), Seq(legalRandomData, legalRandomData), Seq(0x0f.U, 0x0f.U))
//      store_enq(Seq(0x080.U, 0x080.U), Seq(legalRandomData, legalRandomData), Seq(0x0f.U, 0x0f.U))
//      store_enq(Seq(0x0c0.U, 0x0c0.U), Seq(legalRandomData, legalRandomData), Seq(0x0f.U, 0x0f.U))
//      store_enq(Seq(0x100.U, 0x100.U), Seq(legalRandomData, legalRandomData), Seq(0x0f.U, 0x0f.U))
//      store_enq(Seq(0x140.U, 0x140.U), Seq(legalRandomData, legalRandomData), Seq(0x0f.U, 0x0f.U))
//      store_enq(Seq(0x180.U, 0x180.U), Seq(legalRandomData, legalRandomData), Seq(0x0f.U, 0x0f.U))
//      store_enq(Seq(0x1c0.U, 0x1c0.U), Seq(legalRandomData, legalRandomData), Seq(0x0f.U, 0x0f.U))
//      store_enq(Seq(0x200.U, 0x200.U), Seq(legalRandomData, legalRandomData), Seq(0x0f.U, 0x0f.U))
//      store_enq(Seq(0x240.U, 0x240.U), Seq(legalRandomData, legalRandomData), Seq(0x0f.U, 0x0f.U))
//      store_enq(Seq(0x280.U, 0x280.U), Seq(legalRandomData, legalRandomData), Seq(0x0f.U, 0x0f.U))
//      store_enq(Seq(0x2c0.U, 0x2c0.U), Seq(legalRandomData, legalRandomData), Seq(0x0f.U, 0x0f.U))
//
//      c.clock.step(2)
//      c.io.dcache.req.ready.poke(true.B)
//      store_enq(Seq(0x000.U, 0x000.U), Seq(hiData.U, hiData.U), Seq(0xf0.U, 0xf0.U),false)
//      c.clock.step(2)
//      forward_req_and_resp(Seq(0x000.U, 0x000.U), Seq(expectedData.U, expectedData.U), Seq(0xff.U, 0xff.U))
//    }
//  }
}

