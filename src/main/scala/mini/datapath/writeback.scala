package mini.Datapath

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import mini.common._
import mini.common.RISCVConstants._
import mini.{CoreConfig, RegFile}

import mini.Control._
import CPUControlSignalTypes._
import mini.{ForwardDecOperand, ForwardExeOperand}

class WritebackStageIO(xlen: Int) extends Bundle {
  val mw_reg = Input(new MemoryWritebackPipelineRegister(xlen))
  val wb_data = Output(UInt(xlen.W))
}

class WritebackStage(val conf: CoreConfig) extends Module {
  val io = IO(new WritebackStageIO(conf.xlen))

//   IO connection to regfile
  io.wb_data := io.mw_reg.wb_data
}
