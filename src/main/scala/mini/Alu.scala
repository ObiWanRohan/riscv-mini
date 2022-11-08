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

// class AluIO(width: Int) extends Bundle {
//   val A = Input(UInt(width.W))
//   val B = Input(UInt(width.W))
//   val alu_op = Input(AluSel())
//   val out = Output(UInt(width.W))
//   val sum = Output(UInt(width.W))
// }
class AluStageIO(width: Int) extends Bundle {
  val A = Input(UInt(width.W))
  val B = Input(UInt(width.W))
  val Cin = Input(UInt(1.W))
  val alu_op = Input(AluSel())
  val Out = Output(UInt(width.W))
  val Cout = Output(UInt(1.W))
  val sum = Output(UInt(width.W))
}

import mini.AluSel._

trait Alu extends Module {
  def width: Int
  val io: AluStageIO
}

// class AluSimple(val width: Int) extends Alu {
//   val io = IO(new AluIO(width))

//   val shamt = io.B(4, 0).asUInt

//   io.out := MuxLookup(
//     io.alu_op.asUInt,
//     io.B,
//     Seq(
// ALU_ADD.asUInt -> (io.A + io.B),
// ALU_SUB.asUInt -> (io.A - io.B),
// ALU_SRA.asUInt -> (io.A.asSInt >> shamt).asUInt,
// ALU_SRL.asUInt -> (io.A >> shamt),
// ALU_SLL.asUInt -> (io.A << shamt),
// ALU_SLT.asUInt -> (io.A.asSInt < io.B.asSInt),
// ALU_SLTU.asUInt -> (io.A < io.B),
// ALU_AND.asUInt -> (io.A & io.B),
// ALU_OR.asUInt -> (io.A | io.B),
// ALU_XOR.asUInt -> (io.A ^ io.B),
// ALU_COPY_A.asUInt -> io.A
//     )
//   )

//   io.sum := io.A + Mux(io.alu_op.asUInt(0), -io.B, io.B)
// }

// class AluArea(val width: Int) extends Alu {
//   val io = IO(new AluIO(width))
//   val sum = io.A + Mux(io.alu_op.asUInt(0), -io.B, io.B)
//   val cmp =
//     Mux(io.A(width - 1) === io.B(width - 1), sum(width - 1), Mux(io.alu_op.asUInt(1), io.B(width - 1), io.A(width - 1)))
//   val shamt = io.B(4, 0).asUInt
//   val shin = Mux(io.alu_op.asUInt(3), io.A, Reverse(io.A))
//   val shiftr = (Cat(io.alu_op.asUInt(0) && shin(width - 1), shin).asSInt >> shamt)(width - 1, 0)
//   val shiftl = Reverse(shiftr)

//   val out =
//     Mux(
//       io.alu_op === ALU_ADD || io.alu_op === ALU_SUB,
//       sum,
//       Mux(
//         io.alu_op === ALU_SLT || io.alu_op === ALU_SLTU,
//         cmp,
//         Mux(
//           io.alu_op === ALU_SRA || io.alu_op === ALU_SRL,
//           shiftr,
//           Mux(
//             io.alu_op === ALU_SLL,
//             shiftl,
//             Mux(
//               io.alu_op === ALU_AND,
//               io.A & io.B,
//               Mux(
//                 io.alu_op === ALU_OR,
//                 io.A | io.B,
//                 Mux(io.alu_op === ALU_XOR, io.A ^ io.B, Mux(io.alu_op === ALU_COPY_A, io.A, io.B))
//               )
//             )
//           )
//         )
//       )
//     )

//   io.out := out
//   io.sum := sum
// }

class AluStage(val width: Int) extends Alu {
  val io = IO(new AluStageIO(width))

  // shift-amount
  val shamt = io.B(4, 0).asUInt
  val presult = Wire(UInt(width.W))
  val stages = List(2, 5)

  // ADD

  val adder = Module(new FastAdderPipelined(width, stages))
  // val sum = Wire(UInt((width).W))
  val cin_sum = Wire(UInt(1.W))
  cin_sum := io.Cin
  val cout_sum = Wire(UInt(1.W))
  adder.io.a := io.A
  adder.io.b := io.B
  adder.io.cin := cin_sum
  io.sum := adder.io.Sum
  cout_sum := adder.io.Cout

  // SUB
  val subtractor = Module(new FastSubtractorPipelined(width, stages))
  val cin_sub = Wire(UInt(1.W))
  cin_sub := io.Cin
  val diff = Wire(UInt((width).W))
  val cout_sub = Wire(UInt(1.W))
  subtractor.io.a := io.A
  subtractor.io.b := io.B
  subtractor.io.cin := cin_sub
  diff := subtractor.io.Diff
  cout_sub := subtractor.io.Cout

  // SRA
  // val shifterRA = new BarrelShifterRightArithmetic(width)
  val shiftRA = Wire(UInt((width).W))
  shiftRA := RegNext((io.A.asSInt >> shamt).asUInt)
  // shifterRA.io.In := io.A
  // shifterRA.io.Shamt := shamt
  // shiftRA := shifterRA.io.Out

  // // SRL
  // val ShifterRL = new BarrelShifterRightLogical(width)
  // val shiftRL = Wire(UInt((width).W))
  // shifterRL.io.In := io.a
  // shifterRL.io.Shamt := shamt
  // shiftRL := shifterRL.io.Out

  // val ShifterLL = new BarrelShifterLeftLogical(width)
  // val shiftLL = Wire(UInt((width).W))
  // shifterLL.io.In := io.a
  // shifterLL.io.Shamt := shamt
  // shiftLL := shifterLL.io.Out

  // val lessThan impliment using subtractor?

  presult := MuxLookup(
    io.alu_op.asUInt,
    io.B,
    Seq(
      // use alu_utils for custom HW implimentaitons -- barrel shifter, parallel-prefix add/sub
      ALU_ADD.asUInt -> (io.sum),
      ALU_SUB.asUInt -> (diff),
      ALU_SRA.asUInt -> (shiftRA),
      ALU_SRL.asUInt -> RegNext((io.A >> shamt)),
      ALU_SLL.asUInt -> RegNext((io.A << shamt)),
      ALU_SLT.asUInt -> RegNext((io.A.asSInt < io.B.asSInt)),
      ALU_SLTU.asUInt -> RegNext((io.A < io.B)),
      ALU_AND.asUInt -> RegNext((io.A & io.B)),
      ALU_OR.asUInt -> RegNext((io.A | io.B)),
      ALU_XOR.asUInt -> RegNext((io.A ^ io.B)),
      ALU_COPY_A.asUInt -> RegNext(io.A)
    )
  )
  io.Out := presult
  io.Cout := presult(width - 1)
  // io.sum := UInt(0.W)
}
