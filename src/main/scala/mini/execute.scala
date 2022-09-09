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

class ExecuteMemoryPipelineRegister(xlen: Int) extends Bundle {
  val inst = chiselTypeOf(Instructions.NOP)
  val pc = UInt(xlen.W)
  val alu = UInt(xlen.W)
  val rs2 = UInt(xlen.W)
  val csr_in = UInt(xlen.W)

  val ctrl = new ControlSignals

}

class ExecuteStageIO(xlen: Int) extends Bundle {
  val de_reg = Input(new DecodeExecutePipelineRegister(xlen))

  val illegal = Output(Bool())
  val pc_check = Output(Bool())
  val em_reg = Output(new ExecuteMemoryPipelineRegister)

}

class ExecuteStage(val conf: CoreConfig) extends Module {

  val io = IO(new DecodeStageIO(conf.xlen))

  // Instanciating modules needed in execute stage
  val alu = Module(conf.makeImmGen(conf.xlen))
  val brCond = Module(conf.makeBrCond(conf.xlen))

  val de_reg = io.de_reg
  val illegal = io.illegal
  val pc_check = io.pc_check
  val em_reg = io.em_reg

  // ctrl^c ctrl^v
  val ex_alu_opA = Wire(UInt(conf.xlen.W))
  val ex_alu_opB = Wire(UInt(conf.xlen.W))
  val ex_rs1 = Wire(UInt(conf.xlen.W))
  val ex_rs2 = Wire(UInt(conf.xlen.W))

  ex_alu_opA := MuxLookup(
    forwardingUnit.io.forward_exe_opA.asUInt,
    de_reg.opA,
    IndexedSeq(
      // This should be the highest priority since it has the latest result
      ForwardExeOperand.FWD_EM.asUInt -> em_reg.alu,
      // Forward from MEM/WB stage register
      ForwardExeOperand.FWD_MW.asUInt -> regWrite,
      ForwardExeOperand.FWD_NONE.asUInt -> de_reg.opA
    )
  )
  ex_alu_opB := MuxLookup(
    forwardingUnit.io.forward_exe_opB.asUInt,
    de_reg.opB,
    IndexedSeq(
      // Forward from MEM/WB stage register
      ForwardExeOperand.FWD_EM.asUInt -> em_reg.alu,
      ForwardExeOperand.FWD_MW.asUInt -> regWrite,
      ForwardExeOperand.FWD_NONE.asUInt -> de_reg.opB
    )
  )

  ex_rs1 := MuxCase(
    de_reg.rs1,
    IndexedSeq(
      // Forward from MEM/WB stage register
      (forwardingUnit.io.forward_exe_rs1 === ForwardExeOperand.FWD_MW) -> regWrite,
      (forwardingUnit.io.forward_exe_rs1 === ForwardExeOperand.FWD_EM) -> em_reg.alu,
      (forwardingUnit.io.forward_exe_rs1 === ForwardExeOperand.FWD_NONE) -> de_reg.rs1
    )
  )

  ex_rs2 := MuxCase(
    de_reg.rs2,
    IndexedSeq(
      // Forward from MEM/WB stage register
      (forwardingUnit.io.forward_exe_rs2 === ForwardExeOperand.FWD_MW) -> regWrite,
      (forwardingUnit.io.forward_exe_rs2 === ForwardExeOperand.FWD_EM) -> em_reg.alu,
      (forwardingUnit.io.forward_exe_rs2 === ForwardExeOperand.FWD_NONE) -> de_reg.rs2
    )
  )
  alu.io.A := ex_alu_opA
  alu.io.B := ex_alu_opB

  alu.io.alu_op := de_reg.ctrl.alu_op

  // Branch condition calc
  brCond.io.rs1 := ex_rs1
  brCond.io.rs2 := ex_rs2
  brCond.io.br_type := de_reg.ctrl.br_type

  // Pipelining

  when(reset.asBool || !fetchStage.io.full_stall && csr.io.exception) {
    pc_check := false.B
    illegal := false.B

  }.elsewhen(!fetchStage.io.full_stall && !csr.io.exception) {
    em_reg.pc := de_reg.pc
    em_reg.inst := de_reg.inst
    em_reg.ctrl := de_reg.ctrl
    em_reg.rs2 := de_reg.rs2
    em_reg.alu := alu.io.out

    illegal := de_reg.ctrl.illegal

    // ew_reg.csr_in := Mux(de_reg.ctrl.imm_sel === ImmSel.IMM_Z, de_reg.immOut, de_reg.opA)
    em_reg.csr_in := alu.io.out

    // Might need to convert this to a wire and make it a MuxLookup
    pc_check := de_reg.ctrl.pc_sel === PCSel.PC_ALU
  }

  forwardingUnit.io.em_reg := em_reg
  // ctrl^c ctrl^v

  // IO connections for execute stafe
  io.em_reg := em_reg;

}
