package mini.Datapath

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import mini.common._
import mini.common.RISCVConstants._
import mini.{Alu, AluSel, CSR, CSRIO, Cache, CacheIO, Const, ControlSignals, CoreConfig, Instructions}

import mini.Control._
import CPUControlSignalTypes._
import mini.ForwardDecOperand._

class MemoryWritebackPipelineRegister(xlen: Int) extends Bundle {
  val inst = chiselTypeOf(Instructions.NOP)
  val pc = UInt(xlen.W)
  val wb_data = UInt(xlen.W) //needed for writeback
  val rs2 = UInt(xlen.W) // needed for writeback
  val dcache_out = SInt(xlen.W) //

  val ctrl = new ControlSignals
}

class MemoryStageIO(xlen: Int) extends Bundle {
  val dcache = Flipped(new CacheIO(xlen, xlen))

  val full_stall = Input(Bool())
  val de_reg = Input(new DecodeExecutePipelineRegister(xlen))
  val em_reg = Input(new ExecuteMemoryPipelineRegister(xlen))

  // TODO: check for critical path
  val ex_rs2 = Input(UInt(xlen.W))
  val alu = Input(new Bundle {
    val sum = UInt(xlen.W)
  })

  val pc_check = Input(Bool())
  val illegal = Input(Bool())

  val host = Output(new Bundle {
    val tohost = UInt(xlen.W)
    val fromhost = UInt(xlen.W)
  })
  val csr = Output(new CSRIO(xlen))
  val mw_reg = Output(new MemoryWritebackPipelineRegister(xlen))
}

class MemoryStage(val conf: CoreConfig) extends Module {
  val io = IO(new MemoryStageIO(conf.xlen))
  val csr = Module(new CSR(conf.xlen)) //mem stage

  val mw_reg = RegInit(
    (new MemoryWritebackPipelineRegister(conf.xlen)).Lit(
      _.inst -> Instructions.NOP,
      _.pc -> 0.U,
      _.wb_data -> 0.U,
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

  val regWrite = Wire(UInt(conf.xlen.W))
  val load = Wire(SInt(conf.xlen.W))
//   Load
  val loffset = (io.em_reg.alu(1) << 4.U).asUInt | (io.em_reg.alu(0) << 3.U).asUInt
  val lshift = io.dcache.resp.bits.data >> loffset
  load := MuxLookup(
    io.em_reg.ctrl.ld_type.asUInt,
    io.dcache.resp.bits.data.zext,
    Seq(
      LdType.LD_LH.asUInt -> lshift(15, 0).asSInt,
      LdType.LD_LB.asUInt -> lshift(7, 0).asSInt,
      LdType.LD_LHU.asUInt -> lshift(15, 0).zext,
      LdType.LD_LBU.asUInt -> lshift(7, 0).zext
    )
  )
  // CSR access -----------------------------VERIFY CSR EXECUTE / MEMORY PIPELINE ACCESS BELOW
  csr.io.stall := io.full_stall
  csr.io.in := io.em_reg.csr_in
  csr.io.cmd := io.em_reg.ctrl.csr_cmd
  csr.io.inst := io.em_reg.inst
  csr.io.pc := io.em_reg.pc
  csr.io.addr := io.em_reg.alu
  csr.io.illegal := io.illegal
  csr.io.pc_check := io.pc_check
  csr.io.ld_type := io.em_reg.ctrl.ld_type
  csr.io.st_type := io.em_reg.ctrl.st_type

  // Temporarily changed for tests
  // io.host <> csr.io.host
  csr.io.host.fromhost.bits := 0.U
  csr.io.host.fromhost.valid := false.B

  val memReqValid =
    !io.full_stall && (io.de_reg.ctrl.st_type.asUInt.orR || io.de_reg.ctrl.ld_type.asUInt.orR)

  // D$ access
  val daddr = Mux(io.full_stall, io.em_reg.alu, io.alu.sum) >> 2.U << 2.U
  val woffset = (io.alu.sum(1) << 4.U).asUInt | (io.alu.sum(0) << 3.U).asUInt

  val tohost_reg = Reg(UInt(conf.xlen.W))
  val tohost_mem_req = memReqValid && io.de_reg.ctrl.st_type.asUInt.orR && daddr === Const.HOST_ADDR.U

  // Use data from memory if the memory request was valid in the previous cycle
  // i.e the data is being written in the current cycle. Otherwise send data from CSR
  io.host.tohost := Mux(RegNext(tohost_mem_req), tohost_reg, csr.io.host.tohost)
  // io.host.tohost := 0.U

  // Writing to host IO
  when(tohost_mem_req) {
    // TODO: add mask here
    // TODO: can we use the value from pipeline reg??
    tohost_reg := io.ex_rs2
  }

  // Note the request being made when the instruction is in the previous stage
  io.dcache.req.valid := !io.full_stall && (io.de_reg.ctrl.st_type.asUInt.orR || io.de_reg.ctrl.ld_type.asUInt.orR)
  io.dcache.req.bits.addr := daddr
  io.dcache.req.bits.data := io.ex_rs2 << woffset
  io.dcache.req.bits.mask := MuxLookup(
    Mux(io.full_stall, io.em_reg.ctrl.st_type, io.de_reg.ctrl.st_type).asUInt,
    "b0000".U,
    Seq(
      StType.ST_SW.asUInt -> "b1111".U,
      StType.ST_SH.asUInt -> ("b11".U << io.alu.sum(1, 0)),
      StType.ST_SB.asUInt -> ("b1".U << io.alu.sum(1, 0))
    )
  )
  // Abort store when there's an excpetion
  io.dcache.abort := csr.io.exception

  // forwardingUnit.io.mw_reg := mw_reg

  // Regfile Write data
  regWrite := MuxLookup(
    io.em_reg.ctrl.wb_sel.asUInt,
    io.em_reg.alu.zext,
    Seq(
      WbSel.WB_MEM.asUInt -> load,
      WbSel.WB_PC4.asUInt -> (io.em_reg.pc + 4.U).zext,
      WbSel.WB_CSR.asUInt -> csr.io.out.zext
    )
  ).asUInt

  // add pipeline stage -- ALUOut, Inst, ctrl
  // Pipelining
  when(reset.asBool || !io.full_stall && csr.io.exception) {
    io.pc_check := false.B
    io.illegal := false.B

  }.elsewhen(!io.full_stall && !csr.io.exception) {
    mw_reg.pc := io.em_reg.pc
    mw_reg.inst := io.em_reg.inst
    mw_reg.ctrl := io.em_reg.ctrl
    mw_reg.rs2 := io.em_reg.rs2

    mw_reg.wb_data := regWrite

    mw_reg.dcache_out := load

    mw_reg := io.mw_reg
    io.csr := csr.io
    // io.em_reg.csr_in := alu.io.out

    io.illegal := io.de_reg.ctrl.illegal

    // Might need to convert this to a wire and make it a MuxLookup
    io.pc_check := io.de_reg.ctrl.pc_sel === PCSel.PC_ALU
  }
}
