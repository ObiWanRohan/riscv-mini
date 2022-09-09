import chisel3.util._
import chisel3.experimental.BundleLiterals._

import mini.common._
import mini.common.RISCVConstants._
import mini.{BrCond, Const, CoreConfig, Instructions}

import Control._
import CPUControlSignalTypes._
import ForwardDecOperand._

class WritebackStageIO(xlen: Int) extends Bundle {
  val em_reg = Input(MemoryWritebackPipelineRegister)
  val regWrite = Output(UInt(xlen.W))
}

class MemoryStage(val conf: CoreConfig) extends Module {
  val io = IO(new WritebackStageIO(conf.xlen))

//   IO connection to regfile
//   regfile.io.wdata := io.regWrite`
}
