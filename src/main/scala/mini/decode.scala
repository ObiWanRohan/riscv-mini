package mini.Datapath

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import mini.common._
import mini.common.RISCVConstants._
import mini.CoreConfig

import Control._
import CPUControlSignalTypes._
import ForwardDecOperand._

class DecodeExecutePipelineRegister(xlen: Int) extends Bundle {
  // outputs to de_reg
  val pc = UInt(xlen.W)
  val inst = chiselTypeOf(Instructions.NOP)
  val immOut = UInt(xlen.W)
  val opA = UInt(xlen.W)
  val opB = UInt(xlen.W)
  val rs1 = UInt(xlen.W)
  val rs2 = UInt(xlen.W)

  val ctrl = new ControlSignals
}

class DecodeStageIO(xlen: Int) extends Bundle {
  // val ctrl = Output(xlen.W)
  val rs1 = Output(UInt(conf.xlen.W))
  val rs2 = Output(UInt(conf.xlen.W))
//   val dpathIO = Output(xlen.W)
  val de_reg = RegInit(new DecodeExecutePipelineRegister(xlen)).Lit(
    _.pc -> 0.U,
    _.inst -> Instructions.NOP,
    _.opA -> 0.U,
    _.opB -> 0.U,
    _.rs2 -> 0.U,
    _.ctrl -> (new ControlSignals).Lit(
      _.pc_sel -> PCSel.PC_4,
      _.A_sel -> ASel.A_RS1,
      _.B_sel -> BSel.B_RS2,
      _.imm_sel -> ImmSel.IMM_X,
      _.alu_op -> AluSel.ALU_XOR,
      _.br_type -> BrType.BR_XXX,
      _.inst_kill -> N.asUInt.asBool,
      _.pipeline_kill -> N.asUInt.asBool,
      _.st_type -> StType.ST_XXX,
      _.ld_type -> LdType.LD_XXX,
      _.wb_sel -> WbSel.WB_ALU,
      _.wb_en -> Y.asUInt.asBool,
      _.csr_cmd -> CSR.N,
      _.illegal -> N
    )
  )
  val fd_reg = Input(FetchStage.io.fd_reg)

}

class DecodeStage(val conf: CoreConfig) extends Module {
  val io = IO(new DecodeStageIO(conf.xlen))
  val regFile = Module(new RegFile(conf.xlen))
  val immGen = Module(conf.makeImmGen(conf.xlen))

  val fd_reg = io.fd_reg
  val de_reg = io.de_reg
  // Connect to control signal IO
  // The instruction from the instruction memory
  io.ctrl.inst := fd_reg.inst

  // regFile read
  // Register number fields from instruction
  val dec_rd_addr = fd_reg.inst(RD_MSB, RD_LSB) // Destination Register Address
  val dec_rs1_addr = fd_reg.inst(RS1_MSB, RS1_LSB) // Source Register 1 Address
  val dec_rs2_addr = fd_reg.inst(RS2_MSB, RS2_LSB) // Source Register 2 Address

  // Connecting register address for read
  regFile.io.raddr1 := dec_rs1_addr
  regFile.io.raddr2 := dec_rs2_addr

  // generate immediates
  immGen.io.inst := fd_reg.inst
  immGen.io.sel := io.ctrl.imm_sel

  // Register read data values including bypass
  // val rs1 = Wire(UInt(conf.xlen.W))
  // val rs2 = Wire(UInt(conf.xlen.W))

  io.rs1 := MuxCase(
    regFile.io.rdata1,
    IndexedSeq(
      // Forward from MEM/WB stage register
      (forwardingUnit.io.forward_dec_opA === ForwardDecOperand.FWD_MW) -> regWrite,
      // Forward from EX/MEM stage register
      (forwardingUnit.io.forward_dec_opA === ForwardDecOperand.FWD_EM) -> em_reg.alu,
      // No forwarding
      (forwardingUnit.io.forward_dec_opA === ForwardDecOperand.FWD_NONE) -> regFile.io.rdata1
    )
  )
  io.rs2 := MuxCase(
    regFile.io.rdata2,
    IndexedSeq(
      // Forward from MEM/WB stage register
      (forwardingUnit.io.forward_dec_opB === ForwardDecOperand.FWD_MW) -> regWrite,
      // Forward from EX/MEM stage register
      (forwardingUnit.io.forward_dec_opB === ForwardDecOperand.FWD_EM) -> em_reg.alu,
      // No forwarding
      (forwardingUnit.io.forward_dec_opB === ForwardDecOperand.FWD_NONE) -> regFile.io.rdata2
    )
  )

  // Get new instruction only when a branch/jump is not happening
  val fetch_kill = (
    de_reg.ctrl.inst_kill // Instruction needs to insert a bubble
      || brCond.io.taken // Branch instruction executed and branch taken
      || de_reg.ctrl.pc_sel === PCSel.PC_ALU // Jump instruction executed
  )

  when(!full_stall) {
    de_reg.pc := fd_reg.pc

    when(!dec_stall && !dec_kill) {
      de_reg.inst := fd_reg.inst
      de_reg.ctrl := io.ctrl

      de_reg.rs1 := rs1
      de_reg.rs2 := rs2
      de_reg.immOut := immGen.io.out

      // Mux to bypass register from writeback stage
      de_reg.opA := MuxCase(
        rs1,
        IndexedSeq(
          (io.ctrl.A_sel === ASel.A_RS1) -> rs1,
          (io.ctrl.A_sel === ASel.A_PC) -> fd_reg.pc
        )
      )

      de_reg.opB := MuxCase(
        rs2,
        IndexedSeq(
          (io.ctrl.B_sel === BSel.B_RS2) -> rs2,
          (io.ctrl.B_sel === BSel.B_IMM) -> immGen.io.out
        )
      )

    }.otherwise {
      // Insert NOP when Decode is stalled
      // Advance instruction from Fetch stage

      de_reg.inst := Instructions.NOP

      // TODO: Find a more optimal way for this than hardcoding
      // Manually hardcoded to control signals for Bubble instruction (XOR x0, x0, x0)
      de_reg.ctrl := (new ControlSignals).Lit(
        _.pc_sel -> PCSel.PC_4,
        _.A_sel -> ASel.A_RS1,
        _.B_sel -> BSel.B_RS2,
        _.imm_sel -> ImmSel.IMM_X,
        _.alu_op -> AluSel.ALU_XOR,
        _.br_type -> BrType.BR_XXX,
        _.inst_kill -> N.asUInt.asBool,
        _.pipeline_kill -> N.asUInt.asBool,
        _.st_type -> StType.ST_XXX,
        _.ld_type -> LdType.LD_XXX,
        _.wb_sel -> WbSel.WB_ALU,
        _.wb_en -> Y.asUInt.asBool,
        _.csr_cmd -> CSR.N,
        _.illegal -> N
      )

      de_reg.rs1 := 0.U
      de_reg.rs2 := 0.U
      de_reg.immOut := 0.U
      de_reg.opA := 0.U
      de_reg.opB := 0.U

    }
  }

  val de_rs1_addr = de_reg.inst(RS1_MSB, RS1_LSB)
  val de_rs2_addr = de_reg.inst(RS2_MSB, RS2_LSB)

  forwardingUnit.io.de_reg := de_reg

}
