package mini.common

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.internal.sourceinfo.SourceInfo

class SplitUInt(val xlen: Int, val numWays: Int, init: Option[UInt] = None) extends Bundle with Num[SplitUInt] {

  def this(xlen: Int, numWays: Int, init: UInt) = {
    this(xlen, numWays, Some(init))
  }

  // Check power of 2
  require((numWays & (numWays - 1)) == 0, s"SplitInt numWays needs to be a power of 2. Got $numWays")

  // Width of each subpart
  val subdataWidth = xlen / numWays

  val data: Vec[UInt] = if (init.isEmpty) {

    Vec(numWays, UInt(subdataWidth.W))
  } else {

    require(init.get.getWidth == xlen, "The initialisation vector should have the same width")

    VecInit(IntSplitter(init.get, numWays))
  }

  // For direct indexing of subdata
  def apply(idx: Int): UInt = {
    // print(s"Returning index $idx\n")
    data(idx)
  }

  def toUInt: UInt = Cat(data.reverse)
  override def do_asUInt(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt = this.toUInt

  def toBits: Seq[Bits] = this.toUInt.asBools

  def ===(that: SplitUInt): Bool = {

    this.data.zip(that.data).map({ case (x, y) => x === y }).reduce((x, y) => x && y)

  }

  def ===(that: UInt): Bool = {

    this.asUInt === that
  }

  def &(that: SplitUInt): SplitUInt = {

    require(
      this.numWays == that.numWays,
      s"Cannot assign a ${this.xlen}-way SplitUInt from a ${that.xlen}-way SplitUInt."
    )

    SplitUInt(this.asUInt & that.asUInt, this.numWays)

  }
  def &(that: UInt): SplitUInt = {

    require(
      this.xlen == that.getWidth,
      s"Cannot assign a ${(this.xlen)} width SplitUInt from a ${this.xlen} bit SplitUInt."
    )

    SplitUInt(this.asUInt & that, this.numWays)

  }

  def :=(that: UInt): Unit = {
    require(
      this.xlen == that.getWidth,
      s"Cannot assign a SplitUInt of width ${this.xlen} from a UInt of different length ${that.getWidth}."
    )

    for ((a, b) <- this.data.zip(IntSplitter(that, this.numWays)))
      a := b

  }

  def :=(that: SplitUInt): Unit = {
    require(
      this.xlen == that.xlen,
      s"Cannot assign a SplitUInt of width ${this.xlen} from a SplitUInt of different length ${that.xlen}."
    )

    // TODO: Rethink this requirement
    require(
      this.numWays == that.numWays,
      s"Cannot assign a ${this.xlen}-way SplitUInt from a ${that.xlen}-way SplitUInt."
    )

    for ((a, b) <- this.data.zip(that.data))
      a := b

  }

  def >>(shamt: Int): SplitUInt = {
    require(shamt > 0, s"$shamt should be greater than 0")
    SplitUInt(
      Cat(
        0.U(shamt.W),
        (this.toUInt >> shamt)
      ),
      this.numWays
    )

  }

  // TODO: Add a shifter here
  def >>(shamt: UInt): SplitUInt = {

    SplitUInt(
      Cat(
        0.U(shamt.getWidth.W),
        (this.toUInt >> shamt)
      ),
      this.numWays
    )

  }

  def <<(shamt: Int): SplitUInt = {
    require(shamt > 0, s"$shamt should be greater than 0")

    SplitUInt(
      Cat(
        (this.toUInt << shamt)(this.xlen - shamt - 1, shamt),
        0.U(shamt.W)
      ),
      this.numWays
    )
  }

  // TODO: Add a shifter here
  def <<(shamt: UInt): SplitUInt = {

    SplitUInt(
      Cat(
        (this.toUInt << shamt)(this.xlen - shamt.getWidth - 1, shamt.getWidth),
        0.U(shamt.getWidth.W)
      ),
      this.numWays
    )
  }

  def +(that: UInt): SplitUInt = {
    SplitUInt(this.toUInt +% that, numWays)
  }

  override def toPrintable: Printable = this.toUInt.toPrintable

  def do_+(that: SplitUInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SplitUInt = {
    SplitUInt(this.toUInt +% that.toUInt, numWays)
  }

  def do_-(
    that: SplitUInt
  )(
    implicit sourceInfo: SourceInfo,
    compileOptions:      CompileOptions
  ): SplitUInt = SplitUInt(this.toUInt -% that.toUInt, numWays)

  def do_/(
    that: SplitUInt
  )(
    implicit sourceInfo: SourceInfo,
    compileOptions:      CompileOptions
  ): SplitUInt = ???

  def do_>(that: SplitUInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): chisel3.Bool =
    ???

  def do_>=(
    that: SplitUInt
  )(
    implicit sourceInfo: SourceInfo,
    compileOptions:      CompileOptions
  ): chisel3.Bool = ???

  def do_<(that: SplitUInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): chisel3.Bool =
    ???

  def do_<=(
    that: SplitUInt
  )(
    implicit sourceInfo: SourceInfo,
    compileOptions:      CompileOptions
  ): chisel3.Bool = ???

  def do_%(
    that: SplitUInt
  )(
    implicit sourceInfo: SourceInfo,
    compileOptions:      CompileOptions
  ): SplitUInt = ???

  def do_*(
    that: SplitUInt
  )(
    implicit sourceInfo: SourceInfo,
    compileOptions:      CompileOptions
  ): SplitUInt = ???

  def do_abs(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SplitUInt = ???
  // def +(that: SplitUInt): SplitUInt = {

  //   // assert(this.xlen == that.xlen, "Both operands need to have the same width. Got %d and %d", this.xlen, that.xlen)
  //   // assert(
  //   //   this.numWays == that.numWays,
  //   //   "Both operands need to have the same number of ways. Got %d and %d",
  //   //   this.numWays,
  //   //   that.numWays
  //   // )

  //   assert(this.xlen == that.xlen)
  //   assert(this.numWays == that.numWays)

  //   val sum = Wire(chiselTypeOf(that))

  //   (0 until this.numWays).foreach {
  //     case (index) =>
  //       val tempSum = this.data(index) +& that.data(index)
  //       sum.data(index) :=

  //   }

  // }

}

object SplitUInt {
  def apply(xlen: Int, numWays: Int) = {
    val u = new SplitUInt(xlen, numWays)

    u
  }

  def apply(init: UInt, numWays: Int) = {

    val u = new SplitUInt(init.getWidth, numWays, Some(init))

    (0 until numWays).foldLeft(()) {
      case (_, index) =>
        u(index) := init(((index + 1) * u.subdataWidth) - 1, index * u.subdataWidth)
    }

    u
  }

  def literal(init: UInt, numWays: Int) = {
    (new SplitUInt(init.getWidth, numWays)).Lit(_.data -> VecInit(IntSplitter(init, numWays)))
  }
}

class SkewPipeline(val stages: Int) {}
