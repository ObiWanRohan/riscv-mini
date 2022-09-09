// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import mini.common._
import mini.common.RISCVConstants._
import mini.Datapath._
object Const {
  val PC_START = 0x200
  val PC_EVEC = 0x100
  // Memory Mapped IO address
  val HOST_ADDR = 0x1000
}

class DatapathIO(xlen: Int) extends Bundle {
  val host = new HostIO(xlen)
  val icache = Flipped(new CacheIO(xlen, xlen))
  val dcache = Flipped(new CacheIO(xlen, xlen))
  val ctrl = Flipped(new ControlSignalsIO)
}

// class FetchDecodePipelineRegister(xlen: Int) extends Bundle {
//   val inst = chiselTypeOf(Instructions.NOP)
//   val pc = UInt(xlen.W)
//   val ctrl = new ControlSignals
// }

// class DecodeExecutePipelineRegister(xlen: Int) extends Bundle {
//   val pc = UInt(xlen.W)
//   val inst = chiselTypeOf(Instructions.NOP)
//   val opA = UInt(xlen.W)
//   val opB = UInt(xlen.W)
//   val rs1 = UInt(xlen.W)
//   val rs2 = UInt(xlen.W)

//   val immOut = UInt(xlen.W)

//   val ctrl = new ControlSignals

// This held the address for the writeback
// Disabled for now since the address can be taken from the instruction
// val wb = Reg(UInt())
// }

// class ExecuteMemoryPipelineRegister(xlen: Int) extends Bundle {
//   val inst = chiselTypeOf(Instructions.NOP)
//   val pc = UInt(xlen.W)
//   val alu = UInt(xlen.W)
//   val rs2 = UInt(xlen.W)
//   val csr_in = UInt(xlen.W)

//   // val wb_en = ReBool()

//   val ctrl = new ControlSignals
// }

class MemoryWritebackPipelineRegister(xlen: Int) extends Bundle {
  val inst = chiselTypeOf(Instructions.NOP)
  val pc = UInt(xlen.W)
  val wb_data = UInt(xlen.W) //needed for writeback
  val rs2 = UInt(xlen.W) // needed for writeback
  val dcache_out = SInt(xlen.W) //

  val ctrl = new ControlSignals
}

class Datapath(val conf: CoreConfig) extends Module {
  val io = IO(new DatapathIO(conf.xlen))
  val csr = Module(new CSR(conf.xlen)) //mem stage
  // val regFile = Module(new RegFile(conf.xlen)) //decode stage
  val alu = Module(conf.makeAlu(conf.xlen)) //execute stage
  // val immGen = Module(conf.makeImmGen(conf.xlen)) //decode stage
  val brCond = Module(conf.makeBrCond(conf.xlen)) //execute stage
  val forwardingUnit = Module(conf.makeForwardingUnit(conf.xlen)) //datapath

  import Control._
  import CPUControlSignalTypes._
  import ForwardDecOperand._

  /** Pipeline State Registers * */

  // /** *** Fetch / Decode Registers ****
  //   */
  // val fd_reg = RegInit(
  //   (new FetchDecodePipelineRegister(conf.xlen)).Lit(
  //     _.inst -> Instructions.NOP,
  //     _.pc -> 0.U
  //   )
  // )

  /** *** Decode / Execute Registers ****
    */
  // val de_reg = RegInit(
  //   (new DecodeExecutePipelineRegister(conf.xlen)).Lit(
  // _.inst -> Instructions.NOP,
  // _.pc -> 0.U,
  // _.opA -> 0.U,
  // _.opB -> 0.U,
  // _.rs2 -> 0.U,
  // _.ctrl -> (new ControlSignals).Lit(
  //   _.pc_sel -> PCSel.PC_4,
  //   _.A_sel -> ASel.A_RS1,
  //   _.B_sel -> BSel.B_RS2,
  //   _.imm_sel -> ImmSel.IMM_X,
  //   _.alu_op -> AluSel.ALU_XOR,
  //   _.br_type -> BrType.BR_XXX,
  //   _.inst_kill -> N.asUInt.asBool,
  //   _.pipeline_kill -> N.asUInt.asBool,
  //   _.st_type -> StType.ST_XXX,
  //   _.ld_type -> LdType.LD_XXX,
  //   _.wb_sel -> WbSel.WB_ALU,
  //   _.wb_en -> Y.asUInt.asBool,
  //   _.csr_cmd -> CSR.N,
  //   _.illegal -> N
  //     )
  //   )
  // )

  /** *** Execute / Mem Registers ****
    */
  // val em_reg = RegInit(
  //   (new ExecuteMemoryPipelineRegister(conf.xlen)).Lit(
  //     _.inst -> Instructions.NOP,
  //     _.pc -> 0.U,
  //     _.alu -> 0.U,
  //     _.csr_in -> 0.U,
  //     _.ctrl -> (new ControlSignals).Lit(
  //       _.pc_sel -> PCSel.PC_4,
  //       _.A_sel -> ASel.A_RS1,
  //       _.B_sel -> BSel.B_RS2,
  //       _.imm_sel -> ImmSel.IMM_X,
  //       _.alu_op -> AluSel.ALU_XOR,
  //       _.br_type -> BrType.BR_XXX,
  //       _.inst_kill -> N.asUInt.asBool,
  //       _.pipeline_kill -> N.asUInt.asBool,
  //       _.st_type -> StType.ST_XXX,
  //       _.ld_type -> LdType.LD_XXX,
  //       _.wb_sel -> WbSel.WB_ALU,
  //       _.wb_en -> Y.asUInt.asBool,
  //       _.csr_cmd -> CSR.N,
  //       _.illegal -> N
  //     )
  //   )
  // )

  /** *** Mem / Writeback Registers ****
    */
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

  /** **** Control signals ****
    */
  // TODO: Remove the ones which aren't used globally
  // val st_type = Reg(io.ctrl.st_type.cloneType)
  // val ld_type = Reg(io.ctrl.ld_type.cloneType)
  // val wb_sel = Reg(io.ctrl.wb_sel.cloneType)
  // val wb_en = Reg(Bool())
  // val csr_cmd = Reg(io.ctrl.csr_cmd.cloneType)
  // val illegal = Reg(Bool())
  // val pc_check = Reg(Bool())

  /**
    * *** Wires for writeback and load muxes ***
    */

  val regWrite = Wire(UInt(conf.xlen.W))
  val load = Wire(SInt(conf.xlen.W))

  /**
    * *** Fetch Stage ***
    */
  // val started = RegNext(reset.asBool)

  // // Full pipeline stall for when the IMem or DMem is not responding
  // // This can also be called cache_miss_stall
  val full_stall = !io.icache.resp.valid || !io.dcache.resp.valid

  // Decode Stage Stall
  // Can also be called hazard_stall
  val dec_stall = Wire(Bool())

  // Currently only stalling on load hazards
  dec_stall := (
    de_reg.ctrl.ld_type =/= LdType.LD_XXX // Instruction in Execute stage is a load
      && (
        (de_reg.inst(RD_MSB, RD_LSB) === fd_reg.inst(RS1_MSB, RS1_LSB))
          || (de_reg.inst(RD_MSB, RD_LSB) === fd_reg.inst(RS2_MSB, RS2_LSB))
      ) // And instruction in decode stage is reading the loaded value
  )

  // Kill Fetch stage
  // This means the instruction in the fetch stage will not pass to the decode stage
  // and a NOP will be inserted instead
  val if_kill = (
    (de_reg.ctrl.pc_sel =/= PCSel.PC_4)
  ) || (
    csr.io.exception
  ) || (
    brCond.io.taken
  ) // || cs_fencei || RegNext(cs_fencei) // Will add later

  // Kill Decode stage
  // This means the instruction in the decode stage will not pass to the execute stage
  // and a NOP will be inserted instead
  val dec_kill = (
    de_reg.ctrl.pc_sel =/= PCSel.PC_4
  ) || (
    csr.io.exception
  ) || (
    illegal
  ) || (
    brCond.io.taken
  )

  // val pc = RegInit(Const.PC_START.U(conf.xlen.W) - 4.U(conf.xlen.W))
  // // Next Program Counter
  // val next_pc = MuxCase(
  //   pc + 4.U,
  //   IndexedSeq(
  //     (fetchStage.io.full_stall || dec_stall) -> pc,
  //     csr.io.exception -> csr.io.evec,
  //     (de_reg.ctrl.pc_sel === PCSel.PC_EPC) -> csr.io.epc,
  //     ((de_reg.ctrl.pc_sel === PCSel.PC_ALU) || (brCond.io.taken)) -> (alu.io.sum >> 1.U << 1.U),
  //     (de_reg.ctrl.pc_sel === PCSel.PC_0) -> pc
  //   )
  // )

  // pc := next_pc
  // io.icache.req.bits.addr := next_pc
  // io.icache.req.bits.data := 0.U
  // io.icache.req.bits.mask := 0.U
  // io.icache.req.valid := !fetchStage.io.full_stall
  // io.icache.abort := false.B

  // // Pipelining
  // // Only update the instruction when not stalling
  // when(!fetchStage.io.full_stall && !dec_stall) {
  //   fd_reg.pc := pc
  //   when(if_kill) {
  //     fd_reg.inst := Instructions.NOP
  //   }.otherwise {
  //     fd_reg.inst := inst
  //   }
  // }

  // forwardingUnit.io.fd_reg := fd_reg

  /**
    * *** Decode Stage ***
    */

  // Connect to control signal IO
  // The instruction from the instruction memory
  // io.ctrl.inst := fd_reg.inst

  // // regFile read
  // // Register number fields from instruction
  // val dec_rd_addr = fd_reg.inst(RD_MSB, RD_LSB) // Destination Register Address
  // val dec_rs1_addr = fd_reg.inst(RS1_MSB, RS1_LSB) // Source Register 1 Address
  // val dec_rs2_addr = fd_reg.inst(RS2_MSB, RS2_LSB) // Source Register 2 Address

  // // Connecting register address for read
  // regFile.io.raddr1 := dec_rs1_addr
  // regFile.io.raddr2 := dec_rs2_addr

  // // generate immediates
  // immGen.io.inst := fd_reg.inst
  // immGen.io.sel := io.ctrl.imm_sel

  // // Register read data values including bypass
  // val rs1 = Wire(UInt(conf.xlen.W))
  // val rs2 = Wire(UInt(conf.xlen.W))

  rs1 := MuxCase(
    regFile.io.rdata1,
    IndexedSeq(
      // Forward from MEM/WB stage register
      (forwardingUnit.io.forward_dec_opA === ForwardDecOperand.FWD_MW) -> mw_reg.wb_data,
      // Forward from EX/MEM stage register
      (forwardingUnit.io.forward_dec_opA === ForwardDecOperand.FWD_EM) -> em_reg.alu,
      // No forwarding
      (forwardingUnit.io.forward_dec_opA === ForwardDecOperand.FWD_NONE) -> regFile.io.rdata1
    )
  )
  rs2 := MuxCase(
    regFile.io.rdata2,
    IndexedSeq(
      // Forward from MEM/WB stage register
      (forwardingUnit.io.forward_dec_opB === ForwardDecOperand.FWD_MW) -> mw_reg.wb_data,
      // Forward from EX/MEM stage register
      (forwardingUnit.io.forward_dec_opB === ForwardDecOperand.FWD_EM) -> em_reg.alu,
      // No forwarding
      (forwardingUnit.io.forward_dec_opB === ForwardDecOperand.FWD_NONE) -> regFile.io.rdata2
    )
  )

  // when(!fetchStage.io.full_stall) {
  //   de_reg.pc := fd_reg.pc

  //   when(!dec_stall && !dec_kill) {
  //     de_reg.inst := fd_reg.inst
  //     de_reg.ctrl := io.ctrl

  //     de_reg.rs1 := rs1
  //     de_reg.rs2 := rs2
  //     de_reg.immOut := immGen.io.out

  //     // Mux to bypass register from writeback stage
  //     de_reg.opA := MuxCase(
  //       rs1,
  //       IndexedSeq(
  //         (io.ctrl.A_sel === ASel.A_RS1) -> rs1,
  //         (io.ctrl.A_sel === ASel.A_PC) -> fd_reg.pc
  //       )
  //     )

  //     de_reg.opB := MuxCase(
  //       rs2,
  //       IndexedSeq(
  //         (io.ctrl.B_sel === BSel.B_RS2) -> rs2,
  //         (io.ctrl.B_sel === BSel.B_IMM) -> immGen.io.out
  //       )
  //     )

  //   }.otherwise {
  //     // Insert NOP when Decode is stalled
  //     // Advance instruction from Fetch stage

  //     de_reg.inst := Instructions.NOP

  //     // TODO: Find a more optimal way for this than hardcoding
  //     // Manually hardcoded to control signals for Bubble instruction (XOR x0, x0, x0)
  //     de_reg.ctrl := (new ControlSignals).Lit(
  //       _.pc_sel -> PCSel.PC_4,
  //       _.A_sel -> ASel.A_RS1,
  //       _.B_sel -> BSel.B_RS2,
  //       _.imm_sel -> ImmSel.IMM_X,
  //       _.alu_op -> AluSel.ALU_XOR,
  //       _.br_type -> BrType.BR_XXX,
  //       _.inst_kill -> N.asUInt.asBool,
  //       _.pipeline_kill -> N.asUInt.asBool,
  //       _.st_type -> StType.ST_XXX,
  //       _.ld_type -> LdType.LD_XXX,
  //       _.wb_sel -> WbSel.WB_ALU,
  //       _.wb_en -> Y.asUInt.asBool,
  //       _.csr_cmd -> CSR.N,
  //       _.illegal -> N
  //     )

  //     de_reg.rs1 := 0.U
  //     de_reg.rs2 := 0.U
  //     de_reg.immOut := 0.U
  //     de_reg.opA := 0.U
  //     de_reg.opB := 0.U

  //   }
  // }

  // val de_rs1_addr = de_reg.inst(RS1_MSB, RS1_LSB)
  // val de_rs2_addr = de_reg.inst(RS2_MSB, RS2_LSB)

  // forwardingUnit.io.de_reg := de_reg

  val fetchStage = Module(new FetchStage(conf))
  val decodeStage = Module(new DecodeStage(conf))
  val executeStage = Module(new ExecuteStage(conf))
  val memoryStage = Module(new MemoryStage(conf))
  val writebackStage = Module(new WritebackStage(conf))

  /**
    * *** Execute Stage ***
    */

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
      ForwardExeOperand.FWD_MW.asUInt -> mw_reg.wb_data,
      ForwardExeOperand.FWD_NONE.asUInt -> de_reg.opA
    )
  )
  ex_alu_opB := MuxLookup(
    forwardingUnit.io.forward_exe_opB.asUInt,
    de_reg.opB,
    IndexedSeq(
      // Forward from MEM/WB stage register
      ForwardExeOperand.FWD_EM.asUInt -> em_reg.alu,
      ForwardExeOperand.FWD_MW.asUInt -> mw_reg.wb_data,
      ForwardExeOperand.FWD_NONE.asUInt -> de_reg.opB
    )
  )

  ex_rs1 := MuxLookup(
    forwardingUnit.io.forward_exe_rs1.asUInt,
    de_reg.rs1,
    IndexedSeq(
      // This is the highest priority since it has the latest result
      // Forward from EX/MEM stage register
      ForwardExeOperand.FWD_EM.asUInt -> em_reg.alu,
      // Forward from MEM/WB stage register
      ForwardExeOperand.FWD_MW.asUInt -> mw_reg.wb_data,
      ForwardExeOperand.FWD_NONE.asUInt -> de_reg.rs1
    )
  )

  ex_rs2 := MuxLookup(
    forwardingUnit.io.forward_exe_rs2.asUInt,
    de_reg.rs2,
    IndexedSeq(
      // This is the highest priority since it has the latest result
      // Forward from EX/MEM stage register
      ForwardExeOperand.FWD_EM.asUInt -> em_reg.alu,
      // Forward from MEM/WB stage register
      ForwardExeOperand.FWD_MW.asUInt -> mw_reg.wb_data,
      ForwardExeOperand.FWD_NONE.asUInt -> de_reg.rs2
    )
  )

  // // ALU operations
  // alu.io.A := ex_alu_opA
  // alu.io.B := ex_alu_opB

  // alu.io.alu_op := de_reg.ctrl.alu_op

  // // Branch condition calc
  // brCond.io.rs1 := ex_rs1
  // brCond.io.rs2 := ex_rs2
  // brCond.io.br_type := de_reg.ctrl.br_type

  // Pipelining
  // Kill instruction in execute
  val execute_kill = MemoryStage.io.csr.exception

  when(reset.asBool || !full_stall && execute_kill) {
    pc_check := false.B
    illegal := false.B

    em_reg.pc := 0.U
    em_reg.inst := Instructions.NOP
    em_reg.ctrl := (new ControlSignals).Lit(
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
    em_reg.rs2 := 0.U
    em_reg.alu := 0.U
    em_reg.csr_in := 0.U

  }.elsewhen(!full_stall && !execute_kill) {
    em_reg.pc := de_reg.pc
    em_reg.inst := de_reg.inst
    em_reg.ctrl := de_reg.ctrl
    em_reg.rs2 := de_reg.rs2
    em_reg.alu := alu.io.out

  //   illegal := de_reg.ctrl.illegal

  //   // ew_reg.csr_in := Mux(de_reg.ctrl.imm_sel === ImmSel.IMM_Z, de_reg.immOut, de_reg.opA)
  //   em_reg.csr_in := alu.io.out

  //   // Might need to convert this to a wire and make it a MuxLookup
  //   pc_check := de_reg.ctrl.pc_sel === PCSel.PC_ALU
  // }

  // forwardingUnit.io.em_reg := em_reg

  /**
    * Memory stage
    */

  // Load
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

  // CSR access -----------------------------VERIFY CSR EXECUTE / MEMORY PIPELINE ACCESS BELOW
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

  // Temporarily changed for tests
  // io.host <> csr.io.host
  csr.io.host.fromhost.bits := 0.U
  csr.io.host.fromhost.valid := false.B

  val memReqValid =
    !full_stall && (DecodeStage.io.de_reg.ctrl.st_type.asUInt.orR || DecodeStage.io.de_reg.ctrl.ld_type.asUInt.orR)

  val wb_rd_addr = mw_reg.inst(RD_MSB, RD_LSB)

  // D$ access
  val daddr = Mux(fetchStage.io.full_stall, em_reg.alu, alu.io.sum) >> 2.U << 2.U
  val woffset = (alu.io.sum(1) << 4.U).asUInt | (alu.io.sum(0) << 3.U).asUInt

  val tohost_reg = Reg(UInt(conf.xlen.W))

  val tohost_mem_req = memReqValid && de_reg.ctrl.st_type.asUInt.orR && daddr === Const.HOST_ADDR.U

  // Use data from memory if the memory request was valid in the previous cycle
  // i.e the data is being written in the current cycle. Otherwise send data from CSR
  io.host.tohost := Mux(RegNext(tohost_mem_req), tohost_reg, csr.io.host.tohost)

  // Writing to host IO
  when(tohost_mem_req) {
    // TODO, add mask here
    tohost_reg := ex_rs2
  }

  // Note the request being made when the instruction is in the previous stage
  io.dcache.req.valid := !full_stall && (de_reg.ctrl.st_type.asUInt.orR || de_reg.ctrl.ld_type.asUInt.orR)
  io.dcache.req.bits.addr := daddr
  io.dcache.req.bits.data := ex_rs2 << woffset
  io.dcache.req.bits.mask := MuxLookup(
    Mux(fetchStage.io.full_stall, em_reg.ctrl.st_type, de_reg.ctrl.st_type).asUInt,
    "b0000".U,
    Seq(
      StType.ST_SW.asUInt -> "b1111".U,
      StType.ST_SH.asUInt -> ("b11".U << alu.io.sum(1, 0)),
      StType.ST_SB.asUInt -> ("b1".U << alu.io.sum(1, 0))
    )
  )
  // Abort store when there's an excpetion
  io.dcache.abort := csr.io.exception

  forwardingUnit.io.mw_reg := mw_reg

  // Regfile Write data
  regWrite := MuxLookup(
    em_reg.ctrl.wb_sel.asUInt,
    em_reg.alu.zext,
    Seq(
      WbSel.WB_MEM.asUInt -> load,
      WbSel.WB_PC4.asUInt -> (em_reg.pc + 4.U).zext,
      WbSel.WB_CSR.asUInt -> csr.io.out.zext
    )
  ).asUInt

  // add pipeline stage -- ALUOut, Inst, ctrl
  // Pipelining
  when(reset.asBool || !fetchStage.io.full_stall && csr.io.exception) {
    pc_check := false.B
    illegal := false.B

  }.elsewhen(!fetchStage.io.full_stall && !csr.io.exception) {
    mw_reg.pc := em_reg.pc
    mw_reg.inst := em_reg.inst
    mw_reg.ctrl := em_reg.ctrl
    mw_reg.rs2 := em_reg.rs2

    mw_reg.wb_data := regWrite

    mw_reg.dcache_out := load
    // em_reg.csr_in := alu.io.out

    // illegal := de_reg.ctrl.illegal

    // Might need to convert this to a wire and make it a MuxLookup
    // pc_check := de_reg.ctrl.pc_sel === PCSel.PC_ALU

  }

  /**
    * Writeback stage
    */

  regFile.io.wen := mw_reg.ctrl.wb_en && !full_stall && !csr.io.exception
  regFile.io.waddr := wb_rd_addr
  regFile.io.wdata := mw_reg.wb_data

  forwardingUnit.io.wb_rd := wb_rd_addr
  forwardingUnit.io.wb_en := mw_reg.ctrl.wb_en
  forwardingUnit.io.wb_sel := mw_reg.ctrl.wb_sel

  // TODO: re-enable through AOP
  if (conf.trace) {
    printf(
      "pc=[%x] W[r%d=%x][%d] OpA=[r%d][%x] OpB=[r%d][%x] inst=[%x] %c%c%c DASM(%x)\n",
      em_reg.pc,
      wb_rd_addr,
      regWrite,
      em_reg.ctrl.wb_en && !fetchStage.io.full_stall && !csr.io.exception,
      em_reg.inst(RS1_MSB, RS1_LSB), // RS1 address
      RegNext(de_reg.opA),
      em_reg.inst(RS2_MSB, RS2_LSB), // RS2 address
      RegNext(de_reg.opB),
      em_reg.inst,
      Mux(io.ctrl.inst_kill, Str("K"), Str(" ")),
      MuxLookup(
        em_reg.ctrl.pc_sel.asUInt,
        Str("?"),
        Seq(
          PCSel.PC_4.asUInt -> MuxCase(Str(" "), IndexedSeq((em_reg.ctrl.br_type =/= BrType.BR_XXX) -> Str("B"))),
          PCSel.PC_ALU.asUInt -> Str("R"),
          PCSel.PC_EPC.asUInt -> Str("E")
        )
      ),
      Mux(csr.io.exception, Str("X"), Str(" ")),
      em_reg.inst
    )
  }
}
