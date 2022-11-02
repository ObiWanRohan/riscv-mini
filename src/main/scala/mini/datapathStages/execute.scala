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

class ExecuteMemoryPipelineRegister(xlen: Int) extends Bundle {
  val inst = chiselTypeOf(Instructions.NOP)
  val pc = UInt(xlen.W)
  val alu = UInt(xlen.W)
  val rs2 = UInt(xlen.W)
  val csr_in = UInt(xlen.W)

  val ctrl = new ControlSignals

}

class ExecuteStageIO(xlen: Int) extends Bundle {
  val full_stall = Input(Bool())
  val mem_stage_stall = Input(Bool())

  val de_reg = Input(new DecodeExecutePipelineRegister(xlen))
  val mw_reg = Input(new MemoryWritebackPipelineRegister(xlen))

  val csr = Input(new CSRIOOutput(xlen))

  val forwardSignals = Input(new Bundle {
    val forward_exe_opA = ForwardExeOperand()
    val forward_exe_opB = ForwardExeOperand()
    val forward_exe_rs1 = ForwardExeOperand()
    val forward_exe_rs2 = ForwardExeOperand()
  })

  val em_reg = Output(new ExecuteMemoryPipelineRegister(xlen))

  val brCond = Output(new Bundle {
    val taken = Bool()
  })

  val ex_rs2 = Output(UInt(xlen.W))
  val alu = Output(new Bundle {
    val sum = UInt(xlen.W)
  })

}

class ExecuteStage0(val conf: CoreConfig) extends Module {
  val io = IO(new ExecuteStageIO(conf.xlen))

  // Instanciating modules needed in execute stage
  val alu0 = Module(conf.makeAluHalf(conf.xlen / 2))

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
  )
  ex_alu_opB := MuxLookup(
    io.forwardSignals.forward_exe_opB.asUInt,
    io.de_reg.opB,
    IndexedSeq(
      // Forward from MEM/WB stage register
      ForwardExeOperand.FWD_EM.asUInt -> em_reg.alu,
      ForwardExeOperand.FWD_MW.asUInt -> io.mw_reg.wb_data,
      ForwardExeOperand.FWD_NONE.asUInt -> io.de_reg.opB
    )
  )

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
  )

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
  )
  alu0.io.A := ex_alu_opA(15, 0)
  alu0.io.B := ex_alu_opB(15, 0)

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

  // IO connections for execute0 stage
  // io.em_reg := em_reg
  // io.brCond := brCond.io
  io.ex_rs2 := ex_rs2
  io.alu0 := alu0.io

}

class ExecuteStagePipelineRegister(xlen: Int) extends Bundle {
  // need to pipeline all de_reg signals
  // need to store 16-bit half result + carry  generated by the alu0 and give to alu1

  // 16 + 1 bit to store the result + carry (if generated)
  val alu_store = UInt(width / 2 + 1)
  val alu_op = Input(AluSel())

  // store the upper-half(16-bits) of alu_opA and alu_opB
  val alu_opA_buf = Input(UInt(width / 2))
  val alu_opB_buf = Input(UInt(width / 2))

}

class ExecuteStage1(val conf: CoreConfig) extends Module {
  // val io = IO(new ExecuteStageIO(conf.xlen))

  val alu1 = Module(Conf.makeAluHalf(conf.xlen / 2))
  val brCond = Module(conf.makeBrCond(conf.xlen))

  val ex_reg = RegInit(new ExecuteStagePipelineRegister(conf.xlen))

  val em_reg = RegInit(
    new ExecuteMemoryPipelineRegister(conf.xlen).Lit(
      // TODO:
      // give default values .defaults()
      _.inst -> Instructions.NOP,
      _.pc -> 0.U,
      _.alu -> 0.U,
      _.rs2 -> 0.U,
      // _.dcache_out -> 0.S,
      _.ctrl -> ControlSignals.defaultSignals()
    )
  )

  // IO connections for execute1 stage
  io.em_reg := em_reg
  io.brCond := brCond.io

}
