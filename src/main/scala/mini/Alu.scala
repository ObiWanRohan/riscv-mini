// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

object AluSel extends ChiselEnum {
  val ALU_ADD = Value(0.U)
  val ALU_SUB = Value(1.U)
  val ALU_AND = Value(2.U)
  val ALU_OR = Value(3.U)
  val ALU_XOR = Value(4.U)
  val ALU_SLT = Value(5.U)
  val ALU_SLL = Value(6.U)
  val ALU_SLTU = Value(7.U)
  val ALU_SRL = Value(8.U)
  val ALU_SRA = Value(9.U)
  val ALU_COPY_A = Value(10.U)
  val ALU_COPY_B = Value(11.U)
  val ALU_XXX = Value(15.U)
}

class AluIO(width: Int) extends Bundle {
  val A = Input(UInt(width.W))
  val B = Input(UInt(width.W))
  val alu_op = Input(AluSel())
  val out = Output(UInt(width.W))
  val sum = Output(UInt(width.W))
}
class HalfAluIO(width: Int) extends Bundle {
  val A = Input(UInt(width.W))
  val B = Input(UInt(width.W))
  val cin = Input(UInt(1.W))
  val alu_op = Input(AluSel())
  val pout = Output(UInt(width.W))
  val cout = Output(1.W)
}

import mini.AluSel._

trait Alu extends Module {
  def width: Int
  val io: AluIO
}

class AluSimple(val width: Int) extends Alu {
  val io = IO(new AluIO(width))

  val shamt = io.B(4, 0).asUInt

  io.out := MuxLookup(
    io.alu_op.asUInt,
    io.B,
    Seq(
      ALU_ADD.asUInt -> (io.A + io.B),
      ALU_SUB.asUInt -> (io.A - io.B),
      ALU_SRA.asUInt -> (io.A.asSInt >> shamt).asUInt,
      ALU_SRL.asUInt -> (io.A >> shamt),
      ALU_SLL.asUInt -> (io.A << shamt),
      ALU_SLT.asUInt -> (io.A.asSInt < io.B.asSInt),
      ALU_SLTU.asUInt -> (io.A < io.B),
      ALU_AND.asUInt -> (io.A & io.B),
      ALU_OR.asUInt -> (io.A | io.B),
      ALU_XOR.asUInt -> (io.A ^ io.B),
      ALU_COPY_A.asUInt -> io.A
    )
  )

  io.sum := io.A + Mux(io.alu_op.asUInt(0), -io.B, io.B)
}

class AluArea(val width: Int) extends Alu {
  val io = IO(new AluIO(width))
  val sum = io.A + Mux(io.alu_op.asUInt(0), -io.B, io.B)
  val cmp =
    Mux(io.A(width - 1) === io.B(width - 1), sum(width - 1), Mux(io.alu_op.asUInt(1), io.B(width - 1), io.A(width - 1)))
  val shamt = io.B(4, 0).asUInt
  val shin = Mux(io.alu_op.asUInt(3), io.A, Reverse(io.A))
  val shiftr = (Cat(io.alu_op.asUInt(0) && shin(width - 1), shin).asSInt >> shamt)(width - 1, 0)
  val shiftl = Reverse(shiftr)

  val out =
    Mux(
      io.alu_op === ALU_ADD || io.alu_op === ALU_SUB,
      sum,
      Mux(
        io.alu_op === ALU_SLT || io.alu_op === ALU_SLTU,
        cmp,
        Mux(
          io.alu_op === ALU_SRA || io.alu_op === ALU_SRL,
          shiftr,
          Mux(
            io.alu_op === ALU_SLL,
            shiftl,
            Mux(
              io.alu_op === ALU_AND,
              io.A & io.B,
              Mux(
                io.alu_op === ALU_OR,
                io.A | io.B,
                Mux(io.alu_op === ALU_XOR, io.A ^ io.B, Mux(io.alu_op === ALU_COPY_A, io.A, io.B))
              )
            )
          )
        )
      )
    )

  io.out := out
  io.sum := sum
}

class AluHalf(val width: Int) extends Alu {
  val io = IO(new HalfAluIO(width / 2))

  // shift-amount
  val shamt = io.B(4, 0).asUInt
  // partial alu result
  val presult = Wire(UInt(width / 2 + 1))

  presult := MuxLookup(
    io.alu_op.asUInt,
    io.B,
    Seq(
      // create alu_utils for custom HW implimentaitons
      ALU_ADD.asUInt -> (io.A + io.B + io.cin),
      ALU_SUB.asUInt -> (io.A - io.B),
      ALU_SRA.asUInt -> (io.A.asSInt >> shamt).asUInt,
      ALU_SRL.asUInt -> (io.A >> shamt),
      ALU_SLL.asUInt -> (io.A << shamt),
      ALU_SLT.asUInt -> (io.A.asSInt < io.B.asSInt),
      ALU_SLTU.asUInt -> (io.A < io.B),
      ALU_AND.asUInt -> (io.A & io.B),
      ALU_OR.asUInt -> (io.A | io.B),
      ALU_XOR.asUInt -> (io.A ^ io.B),
      ALU_COPY_A.asUInt -> io.A
    )
  )
  io.pout := presult(15, 0)
  io.cout := presult(16)
}
