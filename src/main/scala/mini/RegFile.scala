// See LICENSE for license details.

package mini

import chisel3._
import chisel3.experimental.BundleLiterals._

import mini.common.RISCVConstants
import mini.common.SplitUInt
import mini.common.IntSplitter

class RegFileIO(xlen: Int) extends Bundle {
  val raddr1 = Input(UInt(5.W))
  val raddr2 = Input(UInt(5.W))
  val rdata1 = Output(UInt(xlen.W))
  val rdata2 = Output(UInt(xlen.W))
  val wen = Input(Bool())
  val waddr = Input(UInt(5.W))
  val wdata = Input(UInt(xlen.W))
}

class RegFileControlIO extends Bundle {
  val raddr1 = UInt(RISCVConstants.REG_ADDR_WIDTH)
  val raddr2 = UInt(RISCVConstants.REG_ADDR_WIDTH)
  val wen = Bool()
  val waddr = UInt(RISCVConstants.REG_ADDR_WIDTH)
}

class RegFile(xlen: Int) extends Module {
  val io = IO(new RegFileIO(xlen))
  val regs = Mem(RISCVConstants.REG_COUNT, UInt(xlen.W))
  io.rdata1 := Mux(io.raddr1.orR, regs(io.raddr1), 0.U)
  io.rdata2 := Mux(io.raddr2.orR, regs(io.raddr2), 0.U)
  when(io.wen & io.waddr.orR) {
    regs(io.waddr) := io.wdata
  }
}

object RegFile {
  def apply(xlen: Int, numWays: Int) = {

    val valency = xlen / numWays

    val regFiles = Seq.fill(numWays)(Module(new RegFile(valency)))

    val signalPipeline = Seq.fill(numWays - 1)(
      RegInit(
        new RegFileIO(xlen).Lit(
          _.raddr1 -> 0.U,
          _.raddr2 -> 0.U,
          _.rdata1 -> 0.U,
          _.rdata2 -> 0.U,
          _.wen -> 0.U,
          _.waddr -> 0.U,
          _.wdata -> 0.U
        )
      )
    )

    signalPipeline.foldLeft(regFiles(0).io) { (x, y) =>
      x := y

      y

    }

  }
}

class SubscalarRegFileIO(xlen: Int, numWays: Int) extends Bundle {
  val raddr1 = Input(UInt(RISCVConstants.REG_ADDR_WIDTH))
  val raddr2 = Input(UInt(RISCVConstants.REG_ADDR_WIDTH))

  val rdata1 = Output(SplitUInt(xlen, numWays))
  val rdata2 = Output(SplitUInt(xlen, numWays))

  val wen = Input(Bool())

  val waddr = Input(UInt(RISCVConstants.REG_ADDR_WIDTH))
  val wdata = Input(SplitUInt(xlen, numWays))
}

/**
  * RegFile optimised for subscalar usage
  *
  * Multiple register files connected in series with parallel outputs
  *
  * @param xlen
  * @param numWays
  */
class SubscalarRegFile(xlen: Int, numWays: Int) extends Module {
  val io = IO(new SubscalarRegFileIO(xlen, numWays))

  val subDataWidth = xlen / numWays
  val initialRegFileIO = Wire(new RegFileControlIO)

  initialRegFileIO := io

  (0 until numWays).foldLeft(initialRegFileIO) {
    case (currentIO, subDataIndex) => {

      // The subdata register file
      val subRegFile = Module(new RegFile(subDataWidth))

      // Register to hold the signals for the next register file
      val nextIOReg = RegInit(
        new RegFileControlIO().Lit(
          _.raddr1 -> 0.U,
          _.raddr2 -> 0.U,
          _.waddr -> 0.U,
          _.wen -> false.B
        )
      )

      // These are control and addresses
      nextIOReg := currentIO

      subRegFile.io.raddr1 := currentIO.raddr1
      subRegFile.io.raddr2 := currentIO.raddr2
      subRegFile.io.wen := currentIO.wen
      subRegFile.io.waddr := currentIO.waddr

      // The data is connected directly to the IO.
      // The control signal pipeline automatically outputs data in the skewed pipeline
      io.rdata1(subDataIndex) := subRegFile.io.rdata1
      io.rdata2(subDataIndex) := subRegFile.io.rdata2
      subRegFile.io.wdata := io.wdata(subDataIndex)

      nextIOReg
    }

  }

}

object SubscalarRegFile {
  def apply(xlen: Int, numWays: Int) = {
    val regFile = Module(new SubscalarRegFile(xlen, numWays))

    regFile
  }
}
