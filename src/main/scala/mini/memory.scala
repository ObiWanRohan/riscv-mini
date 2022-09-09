package mini.Datapath

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import mini.common._
import mini.common.RISCVConstants._
import mini.{Alu, CSR, Cache, Const, CoreConfig, Instructions}

import Control._
import CPUControlSignalTypes._
import ForwardDecOperand._

class MemoryWritebackPipelineRegister(xlen: Int) extends Bundle {
  val inst = chiselTypeOf(Instructions.NOP)
  val pc = UInt(xlen.W)
  val alu = UInt(xlen.W) //needed for writeback
  val rs2 = UInt(xlen.W) // needed for writeback
  val dcache_out = SInt(xlen.W) //

  val ctrl = new ControlSignals
}

class MemoryStageIO(xlen: Int) extends Bundle {
  val dcache = Flipped(new CacheIO(xlen, xlen))

  val alu_sum = Input(UInt(xlen.W))
//   val csr_exeption = Input(Bool())

  val em_reg = Output(MemoryWritebackPipelineRegister)
  val de_reg = Input(DecodeExecutePipelineRegister)
}

class MemoryStage(val conf: CoreConfig) extends Module {
  io = IO(new DecodeStageIO)
  val csr = Module(new CSR(conf.xlen)) //mem stage

  io.alu_sum := AluIO.sum

  val em_reg = RegInit(
    new ExecuteMemoryPipelineRegister(conf.xlen).Lit(
      // TODO:
      // give default values .defaults()
      _.inst -> Instructions.NOP,
      _.pc -> 0.U,
      _.alu -> 0.U,
      _.rs2 -> 0.U,
      _.dcache_out -> 0.S,
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
  )

//   Load
  val loffset = (em_reg.alu(1) << 4.U).asUInt | (em_reg.alu(0) << 3.U).asUInt
  val lshift = io.dcache.resp.bits.data >> loffset
  load := MuxLookup(
    em_reg.ctrl.ld_type.asUInt,
    io.dcache.resp.bits.data.zext,
    Seq(
      LdType.LD_LH.asUInt -> lshift(15, 0).asSInt,
      LdType.LD_LB.asUInt -> lshift(7, 0).asSInt,
      LdType.LD_LHU.asUInt -> lshift(15, 0).zext,
      LdType.LD_LBU.asUInt -> lshift(7, 0).zext
    )
  )

  csr.io.stall := fetchStage.io.full_stall
  csr.io.in := em_reg.csr_in
  csr.io.cmd := em_reg.ctrl.csr_cmd
  csr.io.inst := em_reg.inst
  csr.io.pc := em_reg.pc
  csr.io.addr := em_reg.alu
  csr.io.illegal := illegal
  csr.io.pc_check := pc_check
  csr.io.ld_type := em_reg.ctrl.ld_type
  csr.io.st_type := em_reg.ctrl.st_type
  io.host <> csr.io.host

  val wb_rd_addr = mw_reg.inst(RD_MSB, RD_LSB)

  // D$ access
  val daddr = Mux(full_stall, em_reg.alu, alu_sum) >> 2.U << 2.U
  val woffset = (alu_sum(1) << 4.U).asUInt | (alu_sum(0) << 3.U).asUInt

  // Note the request being made when the instruction is in the previous stage
  io.dcache.req.valid := !full_stall && (de_reg.ctrl.st_type.asUInt.orR || de_reg.ctrl.ld_type.asUInt.orR)
  io.dcache.req.bits.addr := daddr
  io.dcache.req.bits.data := ex_rs2 << woffset
  io.dcache.req.bits.mask := MuxLookup(
    Mux(full_stall, em_reg.ctrl.st_type, de_reg.ctrl.st_type).asUInt,
    "b0000".U,
    Seq(
      StType.ST_SW.asUInt -> "b1111".U,
      StType.ST_SH.asUInt -> ("b11".U << alu_sum(1, 0)),
      StType.ST_SB.asUInt -> ("b1".U << alu_sum(1, 0))
    )
  )
  // Abort store when there's an excpetion
  io.dcache.abort := csr.io.exception

  forwardingUnit.io.mw_reg := mw_reg

  // add pipeline stage -- ALUOut, Inst, ctrl
  // Pipelining
  when(reset.asBool || !full_stall && csr.io.exception) {
    pc_check := false.B
    illegal := false.B

  }.elsewhen(!full_stall && !csr.io.exception) {
    mw_reg.pc := em_reg.pc
    mw_reg.inst := em_reg.inst
    mw_reg.ctrl := em_reg.ctrl
    mw_reg.rs2 := em_reg.rs2
    mw_reg.alu := em_reg.alu
    mw_reg.dcache_out := load
    // em_reg.csr_in := alu.io.out

    // illegal := de_reg.ctrl.illegal

    // Might need to convert this to a wire and make it a MuxLookup
    // pc_check := de_reg.ctrl.pc_sel === PCSel.PC_ALU
  }

}
