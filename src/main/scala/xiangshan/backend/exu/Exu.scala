package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.FunctionUnit._
import xiangshan.backend.fu.{FuConfig, FuOutput, FunctionUnit, HasFuLatency, UncertainLatency}

case class ExuParameters
(
  JmpCnt: Int,
  AluCnt: Int,
  MulCnt: Int,
  MduCnt: Int,
  FmacCnt: Int,
  FmiscCnt: Int,
  FmiscDivSqrtCnt: Int,
  LduCnt: Int,
  StuCnt: Int
) {
  assert(JmpCnt == 1, "Only support 1 JmpUnit now!")

  def IntExuCnt = AluCnt + MulCnt + MduCnt + JmpCnt

  def FpExuCnt = FmacCnt + FmiscCnt + FmiscDivSqrtCnt

  def LsExuCnt = LduCnt + StuCnt

  def ExuCnt = IntExuCnt + FpExuCnt + LduCnt + StuCnt

  def NRFuType = 9

  def FuOpWidth = 7
}

case class ExuConfig
(
  name: String,
  fuConfigs: Seq[FuConfig],
  wbIntPriority: Int,
  wbFpPriority: Int
) {
  def max(in: Seq[Int]): Int = in.reduce((x, y) => if (x > y) x else y)

  val intSrcCnt = max(fuConfigs.map(_.numIntSrc))
  val fpSrcCnt = max(fuConfigs.map(_.numFpSrc))
  val readIntRf = intSrcCnt > 0
  val readFpRf = fpSrcCnt > 0
  val writeIntRf = fuConfigs.map(_.writeIntRf).reduce(_ || _)
  val writeFpRf = fuConfigs.map(_.writeFpRf).reduce(_ || _)
  val hasRedirect = fuConfigs.map(_.hasRedirect).reduce(_ || _)

  val latency: HasFuLatency = {
    val lats = fuConfigs.map(_.latency)
    if (lats.exists(x => x.latencyVal.isEmpty)) {
      UncertainLatency()
    } else {
      val x = lats.head
      for (l <- lats.drop(1)) {
        require(x.latencyVal.get == l.latencyVal.get)
      }
      x
    }
  }
  val hasCertainLatency = latency.latencyVal.nonEmpty
  val hasUncertainlatency = latency.latencyVal.isEmpty

  def canAccept(fuType: UInt): Bool = {
    Cat(fuConfigs.map(_.fuType === fuType)).orR()
  }
}

abstract class Exu(val config: ExuConfig) extends XSModule {

  val supportedFunctionUnits = config.fuConfigs.map(_.fuGen).map(gen => Module(gen()))

  val fuSel = supportedFunctionUnits.zip(config.fuConfigs.map(_.fuSel)).map {
    case (fu, sel) => sel(fu)
  }

  val io = IO(new Bundle() {
    val fromInt = if (config.readIntRf) Flipped(DecoupledIO(new ExuInput)) else null
    val fromFp = if (config.readFpRf) Flipped(DecoupledIO(new ExuInput)) else null
    val redirect = Flipped(ValidIO(new Redirect))
    val toInt = if (config.writeIntRf) DecoupledIO(new ExuOutput) else null
    val toFp = if (config.writeFpRf) DecoupledIO(new ExuOutput) else null
  })

  for ((fuCfg, (fu, sel)) <- config.fuConfigs.zip(supportedFunctionUnits.zip(fuSel))) {

    val in = if (fuCfg.numIntSrc > 0) {
      assert(fuCfg.numFpSrc == 0)
      io.fromInt
    } else {
      assert(fuCfg.numFpSrc > 0)
      io.fromFp
    }

    val src1 = in.bits.src1
    val src2 = in.bits.src2
    val src3 = in.bits.src3

    fu.io.in.valid := in.valid && sel && !in.bits.uop.roqIdx.needFlush(io.redirect)
    fu.io.in.bits.uop := in.bits.uop
    fu.io.in.bits.src.foreach(_ <> DontCare)
    if (fuCfg.srcCnt > 0) {
      fu.io.in.bits.src(0) := src1
    }
    if (fuCfg.srcCnt > 1) {
      fu.io.in.bits.src(1) := src2
    }
    if (fuCfg.srcCnt > 2) {
      fu.io.in.bits.src(2) := src3
    }
    fu.io.redirectIn := io.redirect
  }


  val needArbiter = !(config.latency.latencyVal.nonEmpty && (config.latency.latencyVal.get == 0))

  def writebackArb(in: Seq[DecoupledIO[FuOutput]], out: DecoupledIO[ExuOutput]): Arbiter[FuOutput] = {
    if (needArbiter) {
      val arb = Module(new Arbiter(new FuOutput(in.head.bits.len), in.size))
      arb.io.in <> in
      arb.io.out.ready := out.ready
      out.bits.data := arb.io.out.bits.data
      out.bits.uop := arb.io.out.bits.uop
      out.valid := arb.io.out.valid
      arb
    } else {
      in.foreach(_.ready := out.ready)
      val sel = Mux1H(in.map(x => x.valid -> x))
      out.bits.data := sel.bits.data
      out.bits.uop := sel.bits.uop
      out.valid := sel.valid
      null
    }
  }

  val intArb = if (config.writeIntRf) writebackArb(
    supportedFunctionUnits.zip(config.fuConfigs).filter(x => !x._2.writeFpRf).map(_._1.io.out),
    io.toInt
  ) else null

  val fpArb = if (config.writeFpRf) writebackArb(
    supportedFunctionUnits.zip(config.fuConfigs).filter(x => x._2.writeFpRf).map(_._1.io.out),
    io.toFp
  ) else null

  val readIntFu = config.fuConfigs
    .zip(supportedFunctionUnits.zip(fuSel))
    .filter(_._1.numIntSrc > 0)
    .map(_._2)

  val readFpFu = config.fuConfigs
    .zip(supportedFunctionUnits.zip(fuSel))
    .filter(_._1.numFpSrc > 0)
    .map(_._2)

  def inReady(s: Seq[(FunctionUnit, Bool)]): Bool = {
    if (s.size == 1) {
      s.head._1.io.in.ready
    } else {
      if (needArbiter) {
        Cat(s.map(x => x._1.io.in.ready && x._2)).orR()
      } else {
        Cat(s.map(x => x._1.io.in.ready)).andR()
      }
    }
  }


  if (config.readIntRf) {
    io.fromInt.ready := inReady(readIntFu)
  }

  if (config.readFpRf) {
    io.fromFp.ready := inReady(readFpFu)
  }

  def assignDontCares(out: ExuOutput) = {
    out.brUpdate := DontCare
    out.fflags := DontCare
    out.debug <> DontCare
    out.debug.isMMIO := false.B
    out.redirect <> DontCare
    out.redirectValid := false.B
  }

  if (config.writeFpRf) {
    assignDontCares(io.toFp.bits)
  }
  if (config.writeIntRf) {
    assignDontCares(io.toInt.bits)
  }
}

object Exu {

  val aluExeUnitCfg = ExuConfig("AluExeUnit", Seq(aluCfg), 0, Int.MaxValue)
  val jumpExeUnitCfg = ExuConfig("JmpExeUnit", Seq(jmpCfg, csrCfg, fenceCfg, i2fCfg), 2, Int.MaxValue)
  val mulDivExeUnitCfg = ExuConfig("MulDivExeUnit", Seq(mulCfg, divCfg), 1, Int.MaxValue)
  val fmacExeUnitCfg = ExuConfig("FmacExeUnit", Seq(fmacCfg), Int.MaxValue, 0)
  val fmiscExeUnitCfg = ExuConfig(
    "FmiscExeUnit",
    Seq(f2iCfg, f2fCfg, fdivSqrtCfg),
    Int.MaxValue, 1
  )
  val ldExeUnitCfg = ExuConfig("LoadExu", Seq(lduCfg), wbIntPriority = 0, wbFpPriority = 0)
  val stExeUnitCfg = ExuConfig("StoreExu", Seq(stuCfg, mouCfg), wbIntPriority = Int.MaxValue, wbFpPriority = Int.MaxValue)

  val loadExuConfigs = Seq.fill(exuParameters.LduCnt)(ldExeUnitCfg)
  val storeExuConfigs = Seq.fill(exuParameters.StuCnt)(stExeUnitCfg)

  val intExuConfigs = jumpExeUnitCfg +: (
    Seq.fill(exuParameters.AluCnt)(aluExeUnitCfg) ++
      Seq.fill(exuParameters.MduCnt)(mulDivExeUnitCfg)
  )

  val fpExuConfigs =
    Seq.fill(exuParameters.FmacCnt)(fmacExeUnitCfg) ++
      Seq.fill(exuParameters.FmiscCnt)(fmiscExeUnitCfg)

  val exuConfigs: Seq[ExuConfig] = intExuConfigs ++ fpExuConfigs


}