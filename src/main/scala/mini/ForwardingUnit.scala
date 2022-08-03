package mini

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

// Forwarding options for Decode stage
object ForwardDecOperand extends ChiselEnum {
  // Don't forward anything
  val FWD_NONE = Value(0.U)
  // Forward value from EW pipeline register
  val FWD_EW = Value(2.U)
}

// Forwarding options for Execute stage
object ForwardExeOperand extends ChiselEnum {
  // Don't forward anything
  val FWD_NONE = Value(0.U)
  // Forward value from EW pipeline register
  val FWD_EW = Value(2.U)
}

class ForwardingUnitIO(width: Int) extends Bundle {

  import mini.{DecodeExecutePipelineRegister, ExecuteWritebackPipelineRegister, FetchDecodePipelineRegister}
  import CPUControlSignalTypes._

  // Can add a global constant for the register address width

  val REG_ADDR_WIDTH = 5.W

  /* Inputs from pipeline registers
   */
  val fd_reg = Input(new FetchDecodePipelineRegister(width))
  val de_reg = Input(new DecodeExecutePipelineRegister(width))
  val ew_reg = Input(new ExecuteWritebackPipelineRegister(width))

  // Source register addresses from decode stage

  // Destination register for writeback stage
  val wb_rd = Input(UInt(REG_ADDR_WIDTH))
  // Writeback enable control
  val wb_en = Input(Bool())

  // Writeback selector control
  val wb_sel = Input(WbSel())

  /* Outputs to bypass muxes
   */
  val forward_dec_opA = Output(ForwardDecOperand())
  val forward_dec_opB = Output(ForwardDecOperand())

  val forward_exe_opA = Output(ForwardExeOperand())
  val forward_exe_opB = Output(ForwardExeOperand())

}

class ForwardingUnit(width: Int) extends Module {

  val io = IO(new ForwardingUnitIO(width))
  import CPUControlSignalTypes._

  // Hazard for RS1(ALU OP1) when data is being written to memory in the we,
  // and it is to the same address as the one we have read
  // If data hazard exists, bypass by using the data from pipeline register before writeback stage
  // val rs1hazard = wb_en && rs1_addr.orR && (rs1_addr === wb_rd_addr)
  // val rs2hazard = wb_en && rs2_addr.orR && (rs2_addr === wb_rd_addr)

  val dec_rs1_addr = io.fd_reg.inst(19, 15)
  val dec_rs2_addr = io.fd_reg.inst(24, 20)

  // Condition for forwarding for DE pipeline reg
  // This bypasses the hazard between the Decode and Writeback stage
  // // The writeback stage is writing data to the register file at the same address that we are reading in the same cycle
  // format: off
  io.forward_dec_opA := MuxCase(
    ForwardDecOperand.FWD_NONE,
    IndexedSeq(
      (
        io.wb_rd === dec_rs1_addr    // The destination register is the same that is being read
        && dec_rs1_addr.orR         // The destination register is not register x0
        && io.wb_en                   // Writeback is enabled
        && io.wb_sel === WbSel.WB_ALU       // We are writing the ALU value to the register
      ) -> ForwardDecOperand.FWD_EW
    )
  )
  io.forward_dec_opB := MuxCase(
    ForwardDecOperand.FWD_NONE,
    IndexedSeq(
      (
        io.wb_rd === dec_rs2_addr      // The destination register is the same that is being read
        && dec_rs2_addr.orR           // The destination register is not register x0
        && io.wb_en                     // Writeback is enabled
        && io.wb_sel === WbSel.WB_ALU         // We are writing the ALU value to the register
      ) -> ForwardDecOperand.FWD_EW
    )
  )

  
  val exe_rs1_addr = io.de_reg.inst(19, 15)
  val exe_rs2_addr = io.de_reg.inst(24, 20)

  io.forward_exe_opA := MuxCase(
    ForwardExeOperand.FWD_NONE,
    IndexedSeq(
      (
        io.wb_rd === exe_rs1_addr     // The destination register is the same that is being read
        && exe_rs1_addr.orR          // The destination register is not register x0
        && io.wb_en                   // Writeback is enabled
        && io.wb_sel === WbSel.WB_ALU       // We are writing the ALU value to the register
      ) -> ForwardExeOperand.FWD_EW
    )
  )
  io.forward_exe_opB := MuxCase(
    ForwardExeOperand.FWD_NONE,
    IndexedSeq(
      (
        io.wb_rd === exe_rs2_addr       // The destination register is the same that is being read
        && exe_rs2_addr.orR            // The destination register is not register x0
        && io.wb_en                     // Writeback is enabled
        && io.wb_sel === WbSel.WB_ALU         // We are writing the ALU value to the register
        && io.de_reg.ctrl.B_sel === BSel.B_RS2
      ) -> ForwardExeOperand.FWD_EW
    )
  )

  // format: on

}
