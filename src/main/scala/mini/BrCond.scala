// See LICENSE for license details.

package mini

import chisel3._
import CPUControlSignalTypes._

class BrCondIO(xlen: Int) extends Bundle {
  val rs1 = Input(UInt(xlen.W))
  val rs2 = Input(UInt(xlen.W))
  val br_type = Input(BrType())
  val taken = Output(Bool())
}

trait BrCond extends Module {
  def xlen: Int
  val io: BrCondIO
}

class BrCondSimple(val xlen: Int) extends BrCond {

  import CPUControlSignalTypes.BrType._

  val io = IO(new BrCondIO(xlen))
  val eq = io.rs1 === io.rs2
  val neq = !eq
  val lt = io.rs1.asSInt < io.rs2.asSInt
  val ge = !lt
  val ltu = io.rs1 < io.rs2
  val geu = !ltu
  io.taken :=
    ((io.br_type === BR_EQ) && eq) ||
      ((io.br_type === BR_NE) && neq) ||
      ((io.br_type === BR_LT) && lt) ||
      ((io.br_type === BR_GE) && ge) ||
      ((io.br_type === BR_LTU) && ltu) ||
      ((io.br_type === BR_GEU) && geu)
}

class BrCondArea(val xlen: Int) extends BrCond {

  import CPUControlSignalTypes.BrType._

  val io = IO(new BrCondIO(xlen))
  val diff = io.rs1 - io.rs2
  val neq = diff.orR
  val eq = !neq
  val isSameSign = io.rs1(xlen - 1) === io.rs2(xlen - 1)
  val lt = Mux(isSameSign, diff(xlen - 1), io.rs1(xlen - 1))
  val ltu = Mux(isSameSign, diff(xlen - 1), io.rs2(xlen - 1))
  val ge = !lt
  val geu = !ltu
  io.taken :=
    ((io.br_type === BR_EQ) && eq) ||
      ((io.br_type === BR_NE) && neq) ||
      ((io.br_type === BR_LT) && lt) ||
      ((io.br_type === BR_GE) && ge) ||
      ((io.br_type === BR_LTU) && ltu) ||
      ((io.br_type === BR_GEU) && geu)
}
