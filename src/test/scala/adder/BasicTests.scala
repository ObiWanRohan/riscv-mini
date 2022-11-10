package adder

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

trait AdderTester {
  val makeAdder: Int => Adder
}

trait AdderBehaviour {
  this: AnyFlatSpec with ChiselScalatestTester with AdderTester =>
  def mask(s: Int): Int = (1 << s) - 1

  def testAddition(a: Int, b: Int, s: Int): Unit = {
    val result = (a + b) & mask(s)
    it should s"+ $a, $b and the result == $result" in {

      test(this.makeAdder(s)) { c =>
        c.io.a.poke(a.U(s.W))
        c.io.b.poke(b.U(s.W))
        c.clock.step()
        c.io.sum.expect(result.U(s.W))
      }
    }
  }
}

class BasicAdderTest extends AnyFlatSpec with AdderBehaviour with ChiselScalatestTester with AdderTester {

  val makeAdder = new CarryRippleAdder(_)

  val testData: List[(Int, Int)] = List[(Int, Int)](
    (1, 2),
    (3, 4),
    (4, 5)
  )

  val width = 4

  testData.foreach(data => testAddition(data._1, data._2, width))

  testAddition(0, 0, 1)

}

class SubscalarAdderBasicTest extends AnyFlatSpec with AdderBehaviour with ChiselScalatestTester with AdderTester {

  val numWays = 2
  val makeAdder = new SubscalarAdder(_, numWays)

  val testData: List[(Int, Int)] = List[(Int, Int)](
    (1, 2),
    (3, 4),
    (4, 5)
  )

  val width = 4

  testData.foreach(data => testAddition(data._1, data._2, width))

  testAddition(0, 0, 7)

}

// class SubscalarAdderCascadeTest extends AnyFlatSpec with AdderBehaviour with ChiselScalatestTester with AdderTester {

//   val makeAdder: Int => Adder = {
//     val adder1 = new SubscalarAdder(_ / 2, 4)
//     val adder2 = new SubscalarAdder(_ / 2, 4)

//   }

//   val testData: List[(Int, Int)] = List[(Int, Int)](
//     (1, 2),
//     (3, 4),
//     (4, 5)
//   )

//   val width = 4

//   testData.foreach(data => testAddition(data._1, data._2, width))

//   testAddition(0, 0, 7)

// }
