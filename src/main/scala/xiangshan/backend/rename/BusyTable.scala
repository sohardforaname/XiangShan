package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._
import utils.{ParallelOR, XSDebug}

class BusyTable(numReadPorts: Int, numWritePorts: Int) extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    // set preg state to busy
    val allocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    // set preg state to ready (write back regfile + roq walk)
    val wbPregs = Vec(numWritePorts, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    // read preg state
    val rfReadAddr = Vec(numReadPorts, Input(UInt(PhyRegIdxWidth.W)))
    val pregRdy = Vec(numReadPorts, Output(Bool()))
  })

  val busy_table = RegInit(0.U(NRPhyRegs.W))

  def reqVecToMask(rVec: Vec[Valid[UInt]]): UInt = {
    ParallelOR(rVec.map(v => Mux(v.valid, UIntToOH(v.bits), 0.U)))
  }

  val wbMask = reqVecToMask(io.wbPregs)
  val allocMask = reqVecToMask(io.allocPregs)

  val tableAfterWb = busy_table & (~wbMask).asUInt
  val tableAfterAlloc = tableAfterWb | allocMask

  for((raddr, rdy) <- io.rfReadAddr.zip(io.pregRdy)){
    rdy := !tableAfterWb(raddr)
  }

  busy_table := tableAfterAlloc

//  for((alloc, i) <- io.allocPregs.zipWithIndex){
//    when(alloc.valid){
//      busy_table(alloc.bits) := true.B
//    }
//    XSDebug(alloc.valid, "Allocate %d\n", alloc.bits)
//  }


//  for((wb, i) <- io.wbPregs.zipWithIndex){
//    when(wb.valid){
//      busy_table(wb.bits) := false.B
//    }
//    XSDebug(wb.valid, "writeback %d\n", wb.bits)
//  }

  when(io.flush){
    busy_table := 0.U(NRPhyRegs.W)
  }

  XSDebug(p"busy_table    : ${Binary(busy_table)}\n")
  XSDebug(p"tableNext: ${Binary(tableAfterAlloc)}\n")
  XSDebug(p"allocMask: ${Binary(allocMask)}\n")
  XSDebug(p"wbMask   : ${Binary(wbMask)}\n")
  for (i <- 0 until NRPhyRegs) {
    XSDebug(busy_table(i), "%d is busy\n", i.U)
  }
}


//Predication busy busy_table
class PredBusyTable(numReadPorts: Int, numWritePorts: Int) extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    // set preg state to busy
    val allocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PredWidth.W))))
    // set preg state to ready (write back regfile + roq walk)
    val wbPregs = Vec(numWritePorts, Flipped(ValidIO(UInt(PredWidth.W))))
    // read preg state
    val rfReadAddr = Vec(numReadPorts, Input(UInt(PredWidth.W)))
    val pregRdy = Vec(numReadPorts, Output(Bool()))
  })

  val busy_table = RegInit(0.U(BrqSize.W))

  def reqVecToMask(rVec: Vec[Valid[UInt]]): UInt = {
    ParallelOR(rVec.map(v => Mux(v.valid, UIntToOH(v.bits), 0.U)))
  }

  val wbMask = reqVecToMask(io.wbPregs)
  val allocMask = reqVecToMask(io.allocPregs)

  val tableAfterWb = busy_table & (~wbMask).asUInt
  val tableAfterAlloc = tableAfterWb | allocMask

  for((raddr, rdy) <- io.rfReadAddr.zip(io.pregRdy)){
    rdy := !tableAfterWb(raddr)
  }

  busy_table := tableAfterAlloc

  when(io.flush){
    busy_table := 0.U(PredWidth.W)
  }

  XSDebug(p"[Pred]busy_table    : ${Binary(busy_table)}\n")
  XSDebug(p"[Pred]tableNext: ${Binary(tableAfterAlloc)}\n")
  XSDebug(p"[Pred]allocMask: ${Binary(allocMask)}\n")
  XSDebug(p"[Pred]wbMask   : ${Binary(wbMask)}\n")
  for (i <- 0 until BrqSize) {
    XSDebug(busy_table(i), "[Pred]%d is busy\n", i.U)
  }
}
