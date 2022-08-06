// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._

import CPUControlSignalTypes._

object Control {

  val Y = true.B
  val N = false.B

  import AluSel._
  import Instructions._

  // format: off
  val default =
  //                                                                                            kill                                           wb_en    illegal?
  //                 pc_sel       A_sel        B_sel          imm_sel   alu_op       br_type      |     st_type        ld_type       wb_sel      | csr_cmd |
  //                   |            |            |             |          |              |        |        |              |             |        |   |     |
             List(PCSel.PC_4  , ASel.A_XXX,  BSel.B_XXX, ImmSel.IMM_X, ALU_XXX   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, N, CSR.N, Y)
  val map = Array(
    LUI   -> List(PCSel.PC_4  , ASel.A_PC ,  BSel.B_IMM, ImmSel.IMM_U, ALU_COPY_B, BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    AUIPC -> List(PCSel.PC_4  , ASel.A_PC ,  BSel.B_IMM, ImmSel.IMM_U, ALU_ADD   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    JAL   -> List(PCSel.PC_ALU, ASel.A_PC ,  BSel.B_IMM, ImmSel.IMM_J, ALU_ADD   , BrType.BR_XXX, Y, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_PC4, Y, CSR.N, N),
    JALR  -> List(PCSel.PC_ALU, ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_I, ALU_ADD   , BrType.BR_XXX, Y, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_PC4, Y, CSR.N, N),
    BEQ   -> List(PCSel.PC_4  , ASel.A_PC ,  BSel.B_IMM, ImmSel.IMM_B, ALU_ADD   , BrType.BR_EQ , N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, N, CSR.N, N),
    BNE   -> List(PCSel.PC_4  , ASel.A_PC ,  BSel.B_IMM, ImmSel.IMM_B, ALU_ADD   , BrType.BR_NE , N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, N, CSR.N, N),
    BLT   -> List(PCSel.PC_4  , ASel.A_PC ,  BSel.B_IMM, ImmSel.IMM_B, ALU_ADD   , BrType.BR_LT , N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, N, CSR.N, N),
    BGE   -> List(PCSel.PC_4  , ASel.A_PC ,  BSel.B_IMM, ImmSel.IMM_B, ALU_ADD   , BrType.BR_GE , N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, N, CSR.N, N),
    BLTU  -> List(PCSel.PC_4  , ASel.A_PC ,  BSel.B_IMM, ImmSel.IMM_B, ALU_ADD   , BrType.BR_LTU, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, N, CSR.N, N),
    BGEU  -> List(PCSel.PC_4  , ASel.A_PC ,  BSel.B_IMM, ImmSel.IMM_B, ALU_ADD   , BrType.BR_GEU, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, N, CSR.N, N),
    LB    -> List(PCSel.PC_0  , ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_I, ALU_ADD   , BrType.BR_XXX, Y, StType.ST_XXX, LdType.LD_LB , WbSel.WB_MEM, Y, CSR.N, N),
    LH    -> List(PCSel.PC_0  , ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_I, ALU_ADD   , BrType.BR_XXX, Y, StType.ST_XXX, LdType.LD_LH , WbSel.WB_MEM, Y, CSR.N, N),
    LW    -> List(PCSel.PC_0  , ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_I, ALU_ADD   , BrType.BR_XXX, Y, StType.ST_XXX, LdType.LD_LW , WbSel.WB_MEM, Y, CSR.N, N),
    LBU   -> List(PCSel.PC_0  , ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_I, ALU_ADD   , BrType.BR_XXX, Y, StType.ST_XXX, LdType.LD_LBU, WbSel.WB_MEM, Y, CSR.N, N),
    LHU   -> List(PCSel.PC_0  , ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_I, ALU_ADD   , BrType.BR_XXX, Y, StType.ST_XXX, LdType.LD_LHU, WbSel.WB_MEM, Y, CSR.N, N),
    SB    -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_S, ALU_ADD   , BrType.BR_XXX, N, StType.ST_SB , LdType.LD_XXX, WbSel.WB_ALU, N, CSR.N, N),
    SH    -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_S, ALU_ADD   , BrType.BR_XXX, N, StType.ST_SH , LdType.LD_XXX, WbSel.WB_ALU, N, CSR.N, N),
    SW    -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_S, ALU_ADD   , BrType.BR_XXX, N, StType.ST_SW , LdType.LD_XXX, WbSel.WB_ALU, N, CSR.N, N),
    ADDI  -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_I, ALU_ADD   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    SLTI  -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_I, ALU_SLT   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    SLTIU -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_I, ALU_SLTU  , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    XORI  -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_I, ALU_XOR   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    ORI   -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_I, ALU_OR    , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    ANDI  -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_I, ALU_AND   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    SLLI  -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_I, ALU_SLL   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    SRLI  -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_I, ALU_SRL   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    SRAI  -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_IMM, ImmSel.IMM_I, ALU_SRA   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    ADD   -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_RS2, ImmSel.IMM_X, ALU_ADD   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    SUB   -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_RS2, ImmSel.IMM_X, ALU_SUB   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    SLL   -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_RS2, ImmSel.IMM_X, ALU_SLL   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    SLT   -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_RS2, ImmSel.IMM_X, ALU_SLT   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    SLTU  -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_RS2, ImmSel.IMM_X, ALU_SLTU  , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    XOR   -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_RS2, ImmSel.IMM_X, ALU_XOR   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    SRL   -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_RS2, ImmSel.IMM_X, ALU_SRL   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    SRA   -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_RS2, ImmSel.IMM_X, ALU_SRA   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    OR    -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_RS2, ImmSel.IMM_X, ALU_OR    , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    AND   -> List(PCSel.PC_4  , ASel.A_RS1,  BSel.B_RS2, ImmSel.IMM_X, ALU_AND   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, Y, CSR.N, N),
    FENCE -> List(PCSel.PC_4  , ASel.A_XXX,  BSel.B_XXX, ImmSel.IMM_X, ALU_XXX   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, N, CSR.N, N),
    FENCEI-> List(PCSel.PC_0  , ASel.A_XXX,  BSel.B_XXX, ImmSel.IMM_X, ALU_XXX   , BrType.BR_XXX, Y, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, N, CSR.N, N),
    CSRRW -> List(PCSel.PC_0  , ASel.A_RS1,  BSel.B_XXX, ImmSel.IMM_X, ALU_COPY_A, BrType.BR_XXX, Y, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_CSR, Y, CSR.W, N),
    CSRRS -> List(PCSel.PC_0  , ASel.A_RS1,  BSel.B_XXX, ImmSel.IMM_X, ALU_COPY_A, BrType.BR_XXX, Y, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_CSR, Y, CSR.S, N),
    CSRRC -> List(PCSel.PC_0  , ASel.A_RS1,  BSel.B_XXX, ImmSel.IMM_X, ALU_COPY_A, BrType.BR_XXX, Y, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_CSR, Y, CSR.C, N),
    CSRRWI-> List(PCSel.PC_0  , ASel.A_XXX,  BSel.B_XXX, ImmSel.IMM_Z, ALU_XXX   , BrType.BR_XXX, Y, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_CSR, Y, CSR.W, N),
    CSRRSI-> List(PCSel.PC_0  , ASel.A_XXX,  BSel.B_XXX, ImmSel.IMM_Z, ALU_XXX   , BrType.BR_XXX, Y, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_CSR, Y, CSR.S, N),
    CSRRCI-> List(PCSel.PC_0  , ASel.A_XXX,  BSel.B_XXX, ImmSel.IMM_Z, ALU_XXX   , BrType.BR_XXX, Y, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_CSR, Y, CSR.C, N),
    ECALL -> List(PCSel.PC_4  , ASel.A_XXX,  BSel.B_XXX, ImmSel.IMM_X, ALU_XXX   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_CSR, N, CSR.P, N),
    EBREAK-> List(PCSel.PC_4  , ASel.A_XXX,  BSel.B_XXX, ImmSel.IMM_X, ALU_XXX   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_CSR, N, CSR.P, N),
    ERET  -> List(PCSel.PC_EPC, ASel.A_XXX,  BSel.B_XXX, ImmSel.IMM_X, ALU_XXX   , BrType.BR_XXX, Y, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_CSR, N, CSR.P, N),
    WFI   -> List(PCSel.PC_4  , ASel.A_XXX,  BSel.B_XXX, ImmSel.IMM_X, ALU_XXX   , BrType.BR_XXX, N, StType.ST_XXX, LdType.LD_XXX, WbSel.WB_ALU, N, CSR.N, N))
  // format: on
}

class ControlSignals extends Bundle {

  // ===== Fetch Stage Signals =====
  // Next PC Selector
  val pc_sel = PCSel()

  // ===== Decode Stage Signals =====
  // Immediate type selector
  val imm_sel = ImmSel()
  // Operand A selector
  val A_sel = ASel()
  // Operand B selector
  val B_sel = BSel()

  // ===== Execute Stage Signals =====
  val inst_kill = Bool()
  // Kill the pipeline before the Execute stage (for jump or branch instructions)
  val pipeline_kill = Bool()
  // ALU Operation Selector
  val alu_op = AluSel()
  // Branch Type Selector
  val br_type = BrType()
  // Store Type Selector
  val st_type = StType()
  // Load Type Selector
  val ld_type = LdType()

  // CSR
  val csr_cmd = UInt(3.W)
  val illegal = Bool()

  // ===== Writeback Stage Signals =====
  // Writeback source Selector
  val wb_sel = WbSel()
  // Writeback enable
  val wb_en = Bool()

}
class ControlSignalsIO extends Bundle {

  // Instruction input from fetch stage
  val inst = Input(UInt(32.W))

  // ===== Fetch Stage Signals =====

  // Next PC Selector
  val pc_sel = Output(PCSel())

  // ===== Decode Stage Signals =====
  // Immediate type selector
  val imm_sel = Output(ImmSel())
  // Operand A selector
  val A_sel = Output(ASel())
  // Operand B selector
  val B_sel = Output(BSel())

  // ===== Execute Stage Signals =====
  val inst_kill = Output(Bool())
  // Kill the pipeline before the Execute stage (for jump or branch instructions)
  val pipeline_kill = Output(Bool())
  // ALU Operation Selector
  val alu_op = Output(AluSel())
  // Branch Type Selector
  val br_type = Output(BrType())
  // Store Type Selector
  val st_type = Output(StType())
  // Load Type Selector
  val ld_type = Output(LdType())

  // CSR
  val csr_cmd = Output(UInt(3.W))
  val illegal = Output(Bool())

  // ===== Writeback Stage Signals =====
  // Writeback source Selector
  val wb_sel = Output(WbSel())
  // Writeback enable
  val wb_en = Output(Bool())

}

class Control extends Module {
  val io = IO(new ControlSignalsIO)
  val ctrlSignals = ListLookup(io.inst, Control.default, Control.map)

  // Control signals for Fetch
  io.pc_sel := ctrlSignals(0)
  io.inst_kill := ctrlSignals(6).asUInt.asBool
  io.pipeline_kill := ctrlSignals(6).asUInt.asBool

  // Control signals for Decode
  io.A_sel := ctrlSignals(1)
  io.B_sel := ctrlSignals(2)
  io.imm_sel := ctrlSignals(3)

  // Control signals for Execute
  io.alu_op := ctrlSignals(4)
  io.br_type := ctrlSignals(5)
  io.st_type := ctrlSignals(7)

  // Control signals for Write Back
  io.ld_type := ctrlSignals(8)
  io.wb_sel := ctrlSignals(9)
  io.wb_en := ctrlSignals(10).asUInt.asBool
  io.csr_cmd := ctrlSignals(11)
  io.illegal := ctrlSignals(12)
}
