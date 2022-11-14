package mini.common

import chisel3._
import chisel3.util._

import scala.math._

object Str {
  def apply(s: String): UInt = {
    var i = BigInt(0)
    require(s.forall(validChar _))
    for (c <- s)
      i = (i << 8) | c
    i.asUInt((s.length * 8).W)
  }
  def apply(x: Char): Bits = {
    require(validChar(x))
    val lit = x.asUInt(8.W)
    lit
  }
  def apply(x: UInt): Bits = apply(x, 10)
  def apply(x: UInt, radix: Int): Bits = {
    val rad = radix.U
    val digs = digits(radix)
    val w = x.getWidth
    require(w > 0)

    var q = x
    var s = digs(q % rad)
    for (i <- 1 until ceil(log(2) / log(radix) * w).toInt) {
      q = q / rad
      s = Cat(Mux((radix == 10).B && q === 0.U, Str(' '), digs(q % rad)), s)
    }
    s
  }
  def apply(x: SInt): Bits = apply(x, 10)
  def apply(x: SInt, radix: Int): Bits = {
    val neg = x < 0.S
    val abs = Mux(neg, -x, x).asUInt()
    if (radix != 10) {
      Cat(Mux(neg, Str('-'), Str(' ')), Str(abs, radix))
    } else {
      val rad = radix.U
      val digs = digits(radix)
      val w = abs.getWidth
      require(w > 0)

      var q = abs
      var s = digs(q % rad)
      var needSign = neg
      for (i <- 1 until ceil(log(2) / log(radix) * w).toInt) {
        q = q / rad
        val placeSpace = q === 0.U
        val space = Mux(needSign, Str('-'), Str(' '))
        needSign = needSign && !placeSpace
        s = Cat(Mux(placeSpace, space, digs(q % rad)), s)
      }
      Cat(Mux(needSign, Str('-'), Str(' ')), s)
    }
  }

  def bigIntToString(x: BigInt): String = {
    val s = new StringBuilder
    var b = x
    while (b != 0) {
      s += (x & 0xff).toChar
      b = b >> 8
    }
    s.toString
  }

  private def digit(d:      Int): Char = (if (d < 10) '0' + d else 'a' - 10 + d).toChar
  private def digits(radix: Int): Vec[Bits] =
    VecInit((0 until radix).map(i => Str(digit(i))))

  private def validChar(x: Char) = x == (x & 0xff)
}

object IntSplitter {

  /**
    * Return a Seq containing the UInt split into the number of ways given
    * The first element contains the lower side bytes
    *
    * @param init
    * @param numWays
    * @return
    */
  def apply(init: UInt, numWays: Int): Seq[UInt] = {

    require(
      init.getWidth % numWays == 0,
      s"The UInt should be divisible by the number of ways. ${init.getWidth} is not divisible by $numWays"
    )

    val subdataWidth = init.getWidth / numWays

    (0 until numWays).foldLeft(Seq[UInt]()) { (x, index) =>
      x.appended(init(((index + 1) * subdataWidth) - 1, index * subdataWidth))
    }
  }
}
