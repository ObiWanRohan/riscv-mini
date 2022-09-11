package mini

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

import mini.common.RISCVConstants._

// Forwarding options for Decode stage
object ForwardDecOperand extends ChiselEnum {
  // Don't forward anything
  val FWD_NONE = Value(0.U)
  // Forward value from EM pipeline register
  val FWD_EM = Value(1.U)
  // Forward value from MW pipeline register
  val FWD_MW = Value(2.U)
}

// Forwarding options for Execute stage
object ForwardExeOperand extends ChiselEnum {
  // Don't forward anything
  val FWD_NONE = Value(0.U)
  // Forward value from EM pipeline register
  val FWD_EM = Value(1.U)
  // Forward value from MW pipeline register
  val FWD_MW = Value(2.U)
}

class ForwardingUnitIO(width: Int) extends Bundle {

  import mini.Datapath.{
    DecodeExecutePipelineRegister,
    ExecuteMemoryPipelineRegister,
    FetchDecodePipelineRegister,
    MemoryWritebackPipelineRegister
  }
  import CPUControlSignalTypes._

  /* Inputs from pipeline registers
   */
  val fd_reg = Input(new FetchDecodePipelineRegister(width))
  val de_reg = Input(new DecodeExecutePipelineRegister(width))
  val em_reg = Input(new ExecuteMemoryPipelineRegister(width))
  val mw_reg = Input(new MemoryWritebackPipelineRegister(width))

  // Source register addresses from decode stage

  // Destination register for writeback stage
  val wb_rd = Input(UInt(REG_ADDR_WIDTH))
  // Writeback enable control
  val wb_en = Input(Bool())

  // Writeback selector control
  val wb_sel = Input(WbSel())

  /* Outputs to bypass muxes
   */
  // val forward_dec_opA = Output(ForwardDecOperand())
  // val forward_dec_opB = Output(ForwardDecOperand())

  // val forward_exe_opA = Output(ForwardExeOperand())
  // val forward_exe_opB = Output(ForwardExeOperand())
  // val forward_exe_rs1 = Output(ForwardExeOperand())
  // val forward_exe_rs2 = Output(ForwardExeOperand())

  val forwardSignals = Output(new Bundle {
    val forward_dec_opA = ForwardDecOperand()
    val forward_dec_opB = ForwardDecOperand()
    val forward_exe_opA = ForwardExeOperand()
    val forward_exe_opB = ForwardExeOperand()
    val forward_exe_rs1 = ForwardExeOperand()
    val forward_exe_rs2 = ForwardExeOperand()

  })
}

class ForwardingUnit(width: Int) extends Module {

  val io = IO(new ForwardingUnitIO(width))
  import CPUControlSignalTypes._

  // Hazard for RS1(ALU OP1) when data is being written to memory in the we,
  // and it is to the same address as the one we have read
  // If data hazard exists, bypass by using the data from pipeline register before writeback stage
  // val rs1hazard = wb_en && rs1_addr.orR && (rs1_addr === wb_rd_addr)
  // val rs2hazard = wb_en && rs2_addr.orR && (rs2_addr === wb_rd_addr)

  val dec_rs1_addr = io.fd_reg.inst(RS1_MSB, RS1_LSB)
  val dec_rs2_addr = io.fd_reg.inst(RS2_MSB, RS2_LSB)

  // Condition for forwarding for DE pipeline reg
  // This bypasses the hazard between the Decode and Writeback stage
  // // The writeback stage is writing data to the register file at the same address that we are reading in the same cycle
  // format: off
  io.forwardSignals.forward_dec_opA := MuxCase(
    ForwardDecOperand.FWD_NONE,
    IndexedSeq(
      ( // For bypass from MEM stage to DEC (i.e forwarding the value received from EX stage).
        io.em_reg.inst(RD_MSB, RD_LSB) === dec_rs1_addr    // The destination register is the same that is being read
        && dec_rs1_addr.orR         // The destination register is not register x0
        && io.em_reg.ctrl.wb_en                   // Writeback is enabled
        && io.em_reg.ctrl.wb_sel === WbSel.WB_ALU       // We are writing the ALU value to the register
      ) -> ForwardDecOperand.FWD_EM,
      ( // For bypass from WB stage to DEC. This happens regardless of type of writeback
        io.wb_rd === dec_rs1_addr    // The destination register is the same that is being read
        && dec_rs1_addr.orR         // The destination register is not register x0
        && io.wb_en                   // Writeback is enabled
      ) -> ForwardDecOperand.FWD_MW
    )
  )
  io.forwardSignals.forward_dec_opB := MuxCase(
    ForwardDecOperand.FWD_NONE,
    IndexedSeq(
      ( // For bypass from MEM stage to DEC (i.e forwarding the value received from EX stage).
        io.em_reg.inst(RD_MSB, RD_LSB) === dec_rs2_addr    // The destination register is the same that is being read
        && dec_rs2_addr.orR         // The destination register is not register x0
        && io.em_reg.ctrl.wb_en                   // Writeback is enabled
        && io.em_reg.ctrl.wb_sel === WbSel.WB_ALU       // We are writing the ALU value to the register
      ) -> ForwardDecOperand.FWD_EM,
      (
        io.wb_rd === dec_rs2_addr      // The destination register is the same that is being read
        && dec_rs2_addr.orR           // The destination register is not register x0
        && io.wb_en                     // Writeback is enabled
      ) -> ForwardDecOperand.FWD_MW
    )
  )

  
  val exe_rs1_addr = io.de_reg.inst(RS1_MSB, RS1_LSB)
  val exe_rs2_addr = io.de_reg.inst(RS2_MSB, RS2_LSB)

  io.forwardSignals.forward_exe_opA := MuxCase(
    ForwardExeOperand.FWD_NONE,
    IndexedSeq(
      (
        io.em_reg.inst(RD_MSB, RD_LSB) === exe_rs1_addr
        && exe_rs1_addr.orR
        && io.em_reg.ctrl.wb_en
        && io.em_reg.ctrl.wb_sel === WbSel.WB_ALU // We are writing the ALU value to the register
        && io.de_reg.ctrl.A_sel === ASel.A_RS1
      ) -> ForwardExeOperand.FWD_EM,
      (
        io.wb_rd === exe_rs1_addr     // The destination register is the same that is being read
        && exe_rs1_addr.orR          // The destination register is not register x0
        && io.wb_en                   // Writeback is enabled
        && io.de_reg.ctrl.A_sel === ASel.A_RS1
      ) -> ForwardExeOperand.FWD_MW
    )

    // ADD CONTROL LOGIC FOR LOAD USE HAZARD -- when load mem into reg is followed by use -> one cycle stall(bubble) @exe + bypass path
    // when load mem into reg is follwed by use after 1 cycle delay -- decode stage -- use bypass path
    // in this style of bypass paths, how do we update the ALU.opB to have path from mem stage
  )
  io.forwardSignals.forward_exe_opB := MuxCase(
    ForwardExeOperand.FWD_NONE,
    IndexedSeq(
      (
        io.em_reg.inst(RD_MSB, RD_LSB) === exe_rs2_addr   //dest reg is being read - could be from rs1 or rs2
        && exe_rs2_addr.orR
        && io.em_reg.ctrl.wb_en   // writeback is enabled
        && io.em_reg.ctrl.wb_sel === WbSel.WB_ALU   // we are writing back from memory stage (load)
        && io.de_reg.ctrl.B_sel === BSel.B_RS2
        ) -> ForwardExeOperand.FWD_EM,
      (
        io.wb_rd === exe_rs2_addr       // The destination register is the same that is being read
        && exe_rs2_addr.orR            // The destination register is not register x0
        && io.wb_en                     // Writeback is enabled
        && io.de_reg.ctrl.B_sel === BSel.B_RS2
      ) -> ForwardExeOperand.FWD_MW
    )
  )

  io.forwardSignals.forward_exe_rs1 := MuxCase(
    ForwardExeOperand.FWD_NONE,
    IndexedSeq(
      (
        io.em_reg.inst(RD_MSB, RD_LSB) === exe_rs1_addr   //dest reg is being read - could be from rs1 or rs2
        && exe_rs1_addr.orR
        && io.em_reg.ctrl.wb_en   // writeback is enabled
        && io.em_reg.ctrl.wb_sel === WbSel.WB_ALU   // we are writing back from memory (load)
        ) -> ForwardExeOperand.FWD_EM,
      (
        io.wb_rd === exe_rs1_addr       // The destination register is the same that is being read
        && exe_rs1_addr.orR            // The destination register is not register x0
        && io.wb_en                     // Writeback is enabled
      ) -> ForwardExeOperand.FWD_MW
    )
  )

  io.forwardSignals.forward_exe_rs2 := MuxCase(
    ForwardExeOperand.FWD_NONE,
    IndexedSeq(
      (
        io.em_reg.inst(RD_MSB, RD_LSB) === exe_rs2_addr   //dest reg is being read - could be from rs1 or rs2
        && exe_rs2_addr.orR
        && io.em_reg.ctrl.wb_en   // writeback is enabled
        && io.em_reg.ctrl.wb_sel === WbSel.WB_ALU   // we are writing back from memory (load)
        ) -> ForwardExeOperand.FWD_EM,
      (
        io.wb_rd === exe_rs2_addr       // The destination register is the same that is being read
        && exe_rs2_addr.orR            // The destination register is not register x0
        && io.wb_en                     // Writeback is enabled
      ) -> ForwardExeOperand.FWD_MW
    )
  )

  // format: on

}
