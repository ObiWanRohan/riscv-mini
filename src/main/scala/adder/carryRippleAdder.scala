package adder

import chisel3._
import chisel3.util._

import mini.common.SplitUInt

class AdderIO(width: Int) extends Bundle {
  val a = Input(UInt(width.W))
  val b = Input(UInt(width.W))
  val cin = Input(UInt(1.W))

  val sum = Output(UInt(width.W))
  val cout = Output(UInt(1.W))
}

class FullAdder extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val cin = Input(Bool())
    val sum = Output(Bool())
    val cout = Output(Bool())
  })

  io.sum := io.a ^ io.b ^ io.cin
  io.cout := ((io.a ^ io.b) & io.cin) | (io.a & io.b)

}

trait Adder extends Module {
  def width: Int
  val io: AdderIO
}

class CarryRippleAdder(val width: Int) extends Adder {

  /*
  This Carry Ripple Adder takes in 3 inputs (a, b and cin) and has 2 outputs (sum and cout)
  - a and b are variable width, cin is a single bit
  - sum is the same width as the inputs and cout is a single bit
   */

  val io = IO(new AdderIO(width))

  val outBits = Wire(Vec(width, UInt(1.W)))

  val cout = (0 until width).foldLeft(io.cin) {
    case (carry, index) =>
      val fullAdder = Module(new FullAdder)
      fullAdder.io.a := io.a(index)
      fullAdder.io.b := io.b(index)
      fullAdder.io.cin := carry

      outBits(index) := fullAdder.io.sum
      fullAdder.io.cout // This will be passed as carry to the next interation

  }

  io.sum := outBits.asUInt()
  io.cout := cout

}

class SubscalarAdder(val width: Int, val numWays: Int) extends Adder {
  /*
    This adder computes using numWays banks of adders to compute the sum.
   */

  override val desiredName = f"SubscalarAdder_${numWays}way"

  val io = IO(new AdderIO(width))

  val a = Wire(SplitUInt(width, numWays))
  val b = Wire(SplitUInt(width, numWays))

  val sum = Wire(SplitUInt(width, numWays))
  // Splitting the inputs into the split UInt
  val subdataWidth = width / numWays

  val cout = (0 until numWays).foldLeft(io.cin) {
    case (carry, index) =>
      a(index) := io.a(((index + 1) * subdataWidth) - 1, index * subdataWidth)
      b(index) := io.b(((index + 1) * subdataWidth) - 1, index * subdataWidth)

      val RCA = Module(new CarryRippleAdder(subdataWidth))
      RCA.io.a := a(index)
      RCA.io.b := b(index)
      RCA.io.cin := carry

      sum(index) := RCA.io.sum

      // RegNext for subscalar pipelining
      RegNext(RCA.io.cout)
  }

  // Used for testing the SplitUInt type
  // a := io.a
  // b := io.b
  // sum := a + b
  // io.cout := 0.B

  printf(cf"A : ${a.asUInt()}\n")
  printf(cf"Sum asUInt : ${sum.toUInt} width : $width ways : $numWays\n")
  // io.sum := (a + b)
  io.sum := sum.toUInt
  io.cout := cout

}

class MultipleAdders(width: Int) extends Module {
  val io = IO(new AdderIO(width))

  var sums = Seq.empty[UInt]
  var couts = Seq.empty[UInt]

  for (i <- 0 to 5) {

    val numWays = Math.pow(2, i).toInt
    println(f"Generating ${numWays}-way adder")

    val adder = Module(new SubscalarAdder(width, numWays).suggestName(f"SubscalarAdder_${numWays}"))

    adder.io.a := io.a
    adder.io.b := io.b
    adder.io.cin := io.cin

    // io.cout := adder.io.cout
    // io.sum := adder.io.sum

    sums = sums.appended(adder.io.sum)
    couts = couts.appended(adder.io.cout)

  }

  io.sum := sums.foldLeft(0.U) { (prev, cur) => prev | cur }
  io.cout := couts.foldLeft(0.U) { (prev, cur) => prev | cur }

}
