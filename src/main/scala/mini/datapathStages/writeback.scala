package mini.DatapathStages

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import mini.common._
import mini.common.RISCVConstants._
import mini.{CoreConfig, RegFile}

import CPUControlSignalTypes._
import mini.{ForwardDecOperand, ForwardExeOperand}

class WritebackRegIO(xlen: Int, numWays: Int) extends Bundle {
  // Writeback enable control
  val en = Bool()
  // Destination Register Address
  val rd_addr = UInt(REG_ADDR_WIDTH)
  val data = SplitUInt(xlen, numWays)

}

class WritebackStageIO(xlen: Int, numWays: Int) extends Bundle {
  val mw_reg = Input(new MemoryWritebackPipelineRegister(xlen, numWays))

  val writeback = Output(new WritebackRegIO(xlen, numWays))

}

class WritebackStage(val conf: CoreConfig) extends Module {
  val io = IO(new WritebackStageIO(conf.xlen, conf.numWays))

  val wb_rd_addr = io.mw_reg.inst(RD_MSB, RD_LSB)

//   IO connection to regfile
  io.writeback.data := io.mw_reg.wb_data
  io.writeback.en := io.mw_reg.ctrl.wb_en
  io.writeback.rd_addr := wb_rd_addr
}
