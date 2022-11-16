package mini.DatapathStages

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import mini.common._
import mini.common.RISCVConstants._
import mini.{Alu, AluSel, CSR, CSRIOOutput, CacheIO, ControlSignals, CoreConfig, Instructions, RegFile}
import mini.Control.{N, Y}

import CPUControlSignalTypes._
import mini.{ForwardDecOperand, ForwardExeOperand}

class ExecuteMemoryPipelineRegister(xlen: Int, numWays: Int) extends Bundle {
  val inst = chiselTypeOf(Instructions.NOP)
  val pc = SplitUInt(xlen, numWays)
  val alu = SplitUInt(xlen, numWays)
  val rs2 = SplitUInt(xlen, numWays)
  val csr_in = SplitUInt(xlen, numWays)

  val ctrl = new ControlSignals

}

class ExecuteStageIO(conf: CoreConfig) extends Bundle {
  val full_stall = Input(Bool())
  val mem_stage_stall = Input(Bool())

  val de_reg = Input(new DecodeExecutePipelineRegister(conf.xlen, conf.numWays))
  val mw_reg = Input(new MemoryWritebackPipelineRegister(conf.xlen, conf.numWays))

  val csr = Input(new CSRIOOutput(conf.xlen))

  val forwardSignals = Input(new Bundle {
    val forward_exe_opA = ForwardExeOperand()
    val forward_exe_opB = ForwardExeOperand()
    val forward_exe_rs1 = ForwardExeOperand()
    val forward_exe_rs2 = ForwardExeOperand()
  })

  val em_reg = Output(new ExecuteMemoryPipelineRegister(conf.xlen, conf.numWays))

  val brCond = Output(new Bundle {
    val taken = Bool()
  })

  val ex_rs2 = Output(SplitUInt(conf.xlen, conf.numWays))
  val alu = Output(new Bundle {
    val sum = SplitUInt(conf.xlen, conf.numWays)
  })

}

class ExecuteStage(val conf: CoreConfig) extends Module {
  val io = IO(new ExecuteStageIO(conf))

  // Instanciating modules needed in execute stage
  val alu = Module(conf.makeAlu(conf.xlen))
  val brCond = Module(conf.makeBrCond(conf.xlen))

  val em_reg = RegInit(
    new ExecuteMemoryPipelineRegister(conf.xlen, conf.numWays).Lit(
      // TODO:
      // give default values .defaults()
      _.inst -> Instructions.NOP,
      // _.pc -> 0.U,
      // _.alu -> 0.U,
      // _.rs2 -> 0.U,
      // _.dcache_out -> 0.S,
      _.ctrl -> ControlSignals.defaultSignals()
    )
  )

  // val illegal = io.illegal

  val ex_alu_opA = Wire(UInt(conf.xlen.W))
  val ex_alu_opB = Wire(UInt(conf.xlen.W))
  val ex_rs1 = Wire(UInt(conf.xlen.W))
  val ex_rs2 = Wire(UInt(conf.xlen.W))

  ex_alu_opA := MuxLookup(
    io.forwardSignals.forward_exe_opA.asUInt,
    io.de_reg.opA,
    IndexedSeq(
      // This should be the highest priority since it has the latest result
      ForwardExeOperand.FWD_EM.asUInt -> em_reg.alu,
      // Forward from MEM/WB stage register
      ForwardExeOperand.FWD_MW.asUInt -> io.mw_reg.wb_data,
      ForwardExeOperand.FWD_NONE.asUInt -> io.de_reg.opA
    )
  ).asUInt
  ex_alu_opB := MuxLookup(
    io.forwardSignals.forward_exe_opB.asUInt,
    io.de_reg.opB,
    IndexedSeq(
      // Forward from MEM/WB stage register
      ForwardExeOperand.FWD_EM.asUInt -> em_reg.alu,
      ForwardExeOperand.FWD_MW.asUInt -> io.mw_reg.wb_data,
      ForwardExeOperand.FWD_NONE.asUInt -> io.de_reg.opB
    )
  ).asUInt

  ex_rs1 := MuxLookup(
    io.forwardSignals.forward_exe_rs1.asUInt,
    io.de_reg.rs1,
    IndexedSeq(
      // This is the highest priority since it has the latest result
      // Forward from EX/MEM stage register
      ForwardExeOperand.FWD_EM.asUInt -> em_reg.alu,
      // Forward from MEM/WB stage register
      ForwardExeOperand.FWD_MW.asUInt -> io.mw_reg.wb_data,
      ForwardExeOperand.FWD_NONE.asUInt -> io.de_reg.rs1
    )
  ).asUInt

  ex_rs2 := MuxLookup(
    io.forwardSignals.forward_exe_rs2.asUInt,
    io.de_reg.rs2,
    IndexedSeq(
      // This is the highest priority since it has the latest result
      // Forward from EX/MEM stage register
      ForwardExeOperand.FWD_EM.asUInt -> em_reg.alu,
      // Forward from MEM/WB stage register
      ForwardExeOperand.FWD_MW.asUInt -> io.mw_reg.wb_data,
      ForwardExeOperand.FWD_NONE.asUInt -> io.de_reg.rs2
    )
  ).asUInt
  alu.io.A := ex_alu_opA
  alu.io.B := ex_alu_opB

  alu.io.alu_op := io.de_reg.ctrl.alu_op

  // Branch condition calc
  brCond.io.rs1 := ex_rs1
  brCond.io.rs2 := ex_rs2
  brCond.io.br_type := io.de_reg.ctrl.br_type

  // Pipelining
  // Kill instruction in execute
  val execute_kill = io.csr.exception

  when(reset.asBool || !io.full_stall && execute_kill) {

    em_reg.pc := 0.U
    em_reg.inst := Instructions.NOP
    em_reg.ctrl := (new ControlSignals).Lit(
      _.pc_sel -> PCSel.PC_4,
      _.A_sel -> ASel.A_RS1,
      _.B_sel -> BSel.B_RS2,
      _.imm_sel -> ImmSel.IMM_X,
      _.alu_op -> AluSel.ALU_XOR,
      _.br_type -> BrType.BR_XXX,
      _.inst_kill -> N,
      _.pipeline_kill -> N,
      _.st_type -> StType.ST_XXX,
      _.ld_type -> LdType.LD_XXX,
      _.wb_sel -> WbSel.WB_ALU,
      _.wb_en -> Y,
      _.csr_cmd -> CSR.N,
      _.illegal -> N
    )
    em_reg.rs2 := 0.U
    em_reg.alu := 0.U
    em_reg.csr_in := 0.U

  }.elsewhen(!io.full_stall && !execute_kill && !io.mem_stage_stall) {
    em_reg.pc := io.de_reg.pc
    em_reg.inst := io.de_reg.inst
    em_reg.ctrl := io.de_reg.ctrl
    em_reg.rs2 := io.ex_rs2
    em_reg.alu := alu.io.out

    // ew_reg.csr_in := Mux(io.de_reg.ctrl.imm_sel === ImmSel.IMM_Z, io.de_reg.immOut, io.de_reg.opA)
    em_reg.csr_in := alu.io.out

  }

  // forwardingUnit.io.em_reg := em_reg

  // IO connections for execute stafe
  io.em_reg := em_reg
  io.brCond := brCond.io
  io.ex_rs2 := ex_rs2
  io.alu.sum := alu.io.sum

}
