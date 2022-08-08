// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import mini.common._
import mini.common.RISCVConstants._

object Const {
  val PC_START = 0x200
  val PC_EVEC = 0x100
}

class DatapathIO(xlen: Int) extends Bundle {
  val host = new HostIO(xlen)
  val icache = Flipped(new CacheIO(xlen, xlen))
  val dcache = Flipped(new CacheIO(xlen, xlen))
  val ctrl = Flipped(new ControlSignalsIO)
}

class FetchDecodePipelineRegister(xlen: Int) extends Bundle {
  val inst = chiselTypeOf(Instructions.NOP)
  val pc = UInt(xlen.W)
  val ctrl = new ControlSignals
}

class DecodeExecutePipelineRegister(xlen: Int) extends Bundle {
  val pc = UInt(xlen.W)
  val inst = chiselTypeOf(Instructions.NOP)
  val op1 = UInt(xlen.W)
  val op2 = UInt(xlen.W)
  val rs1 = UInt(xlen.W)
  val rs2 = UInt(xlen.W)

  val immOut = UInt(xlen.W)

  val ctrl = new ControlSignals

  // This held the address for the writeback
  // Disabled for now since the address can be taken from the instruction
  // val wb = Reg(UInt())
}

class ExecuteWritebackPipelineRegister(xlen: Int) extends Bundle {
  val inst = chiselTypeOf(Instructions.NOP)
  val pc = UInt(xlen.W)
  val alu = UInt(xlen.W)
  val rs2 = UInt(xlen.W)
  val csr_in = UInt(xlen.W)

  val ctrl = new ControlSignals
}

class Datapath(val conf: CoreConfig) extends Module {
  val io = IO(new DatapathIO(conf.xlen))
  val csr = Module(new CSR(conf.xlen))
  val regFile = Module(new RegFile(conf.xlen))
  val alu = Module(conf.makeAlu(conf.xlen))
  val immGen = Module(conf.makeImmGen(conf.xlen))
  val brCond = Module(conf.makeBrCond(conf.xlen))
  val forwardingUnit = Module(conf.makeForwardingUnit(conf.xlen))

  import Control._
  import CPUControlSignalTypes._
  import ForwardDecOperand._

  /** Pipeline State Registers * */

  /** *** Fetch / Execute Registers ****
    */
  val fd_reg = RegInit(
    (new FetchDecodePipelineRegister(conf.xlen)).Lit(
      _.inst -> Instructions.NOP,
      _.pc -> 0.U
    )
  )

  /** *** Decode / Execute Registers ****
    */
  val de_reg = RegInit(
    (new DecodeExecutePipelineRegister(conf.xlen)).Lit(
      _.inst -> Instructions.NOP,
      _.pc -> 0.U,
      _.op1 -> 0.U,
      _.op2 -> 0.U,
      _.rs2 -> 0.U
    )
  )

  /** *** Execute / Write Back Registers ****
    */
  val ew_reg = RegInit(
    (new ExecuteWritebackPipelineRegister(conf.xlen)).Lit(
      _.inst -> Instructions.NOP,
      _.pc -> 0.U,
      _.alu -> 0.U,
      _.csr_in -> 0.U
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
  val illegal = Reg(Bool())
  val pc_check = Reg(Bool())

  /**
    * *** Fetch Stage ***
    */
  val started = RegNext(reset.asBool)

  // Full pipeline stall for when the IMem or DMem is not responding
  // This can also be called cache_miss_stall
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
  ) || (
    ew_reg.ctrl.ld_type =/= LdType.LD_XXX // Instruction in Writeback stage is a load
      && (
        (ew_reg.inst(RD_MSB, RD_LSB) === fd_reg.inst(RS1_MSB, RS1_LSB))
          || (ew_reg.inst(RD_MSB, RD_LSB) === fd_reg.inst(RS2_MSB, RS2_LSB))
      ) // And instruction in decode stage is reading the loaded value
  )

  // Kill Fetch stage
  // This means the instruction in the fetch stage will not pass to the decode stage
  // and a NOP will be inserted instead
  val if_kill = (
    (de_reg.ctrl.pc_sel =/= PCSel.PC_4)
    // || cs_fencei || RegNext(cs_fencei) // Will add later
  )
  // Kill Decode stage
  // This means the instruction in the decode stage will not pass to the execute stage
  // and a NOP will be inserted instead
  val dec_kill = (de_reg.ctrl.pc_sel =/= PCSel.PC_4) || illegal

  val pc = RegInit(Const.PC_START.U(conf.xlen.W) - 4.U(conf.xlen.W))
  // Next Program Counter
  val next_pc = MuxCase(
    pc + 4.U,
    IndexedSeq(
      (full_stall || dec_stall) -> pc,
      csr.io.exception -> csr.io.evec,
      (de_reg.ctrl.pc_sel === PCSel.PC_EPC) -> csr.io.epc,
      ((de_reg.ctrl.pc_sel === PCSel.PC_ALU) || (brCond.io.taken)) -> (alu.io.sum >> 1.U << 1.U),
      (de_reg.ctrl.pc_sel === PCSel.PC_0) -> pc
    )
  )

  val inst =
    Mux(started || csr.io.exception, Instructions.NOP, io.icache.resp.bits.data)

  pc := next_pc
  io.icache.req.bits.addr := next_pc
  io.icache.req.bits.data := 0.U
  io.icache.req.bits.mask := 0.U
  io.icache.req.valid := !full_stall
  io.icache.abort := false.B

  // Pipelining
  // Only update the instruction when not stalling
  when(!full_stall && !dec_stall) {
    fd_reg.pc := pc
    when(if_kill) {
      fd_reg.inst := Instructions.NOP
    }.otherwise {
      fd_reg.inst := inst
    }
  }

  forwardingUnit.io.fd_reg := fd_reg

  /**
    * *** Decode Stage ***
    */

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
  val rs1 = Wire(UInt(conf.xlen.W))
  val rs2 = Wire(UInt(conf.xlen.W))

  rs1 := MuxCase(
    regFile.io.rdata1,
    IndexedSeq(
      // Forward from EX/WB stage register
      (forwardingUnit.io.forward_dec_opA === ForwardDecOperand.FWD_EW) -> ew_reg.alu,
      // No forwarding
      (forwardingUnit.io.forward_dec_opA === ForwardDecOperand.FWD_NONE) -> regFile.io.rdata1
    )
  )
  rs2 := MuxCase(
    regFile.io.rdata2,
    IndexedSeq(
      // Forward from EX/WB stage register
      (forwardingUnit.io.forward_dec_opB === ForwardDecOperand.FWD_EW) -> ew_reg.alu,
      // No forwarding
      (forwardingUnit.io.forward_dec_opB === ForwardDecOperand.FWD_NONE) -> regFile.io.rdata2
    )
  )

  // Connect Pipelining Registers

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
      de_reg.op1 := MuxCase(
        rs1,
        IndexedSeq(
          (io.ctrl.A_sel === ASel.A_RS1) -> rs1,
          (io.ctrl.A_sel === ASel.A_PC) -> fd_reg.pc
        )
      )

      de_reg.op2 := MuxCase(
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
      de_reg.op1 := 0.U
      de_reg.op2 := 0.U

    }
  }

  val de_rs1_addr = de_reg.inst(RS1_MSB, RS1_LSB)
  val de_rs2_addr = de_reg.inst(RS2_MSB, RS2_LSB)

  forwardingUnit.io.de_reg := de_reg

  /**
    * *** Execute Stage ***
    */

  val wb_rd_addr = ew_reg.inst(RD_MSB, RD_LSB)

  val ex_alu_op1 = Wire(UInt(conf.xlen.W))
  val ex_alu_op2 = Wire(UInt(conf.xlen.W))
  val ex_rs1 = Wire(UInt(conf.xlen.W))
  val ex_rs2 = Wire(UInt(conf.xlen.W))

  ex_alu_op1 := MuxCase(
    de_reg.op1,
    IndexedSeq(
      (forwardingUnit.io.forward_exe_opA === ForwardExeOperand.FWD_EW) -> ew_reg.alu,
      (forwardingUnit.io.forward_exe_opA === ForwardExeOperand.FWD_NONE) -> de_reg.op1
    )
  )
  ex_alu_op2 := MuxCase(
    de_reg.op2,
    IndexedSeq(
      (forwardingUnit.io.forward_exe_opB === ForwardExeOperand.FWD_EW) -> ew_reg.alu,
      (forwardingUnit.io.forward_exe_opB === ForwardExeOperand.FWD_NONE) -> de_reg.op2
    )
  )

  ex_rs1 := MuxCase(
    de_reg.rs1,
    IndexedSeq(
      (forwardingUnit.io.forward_exe_rs1 === ForwardExeOperand.FWD_EW) -> ew_reg.alu,
      (forwardingUnit.io.forward_exe_rs1 === ForwardExeOperand.FWD_NONE) -> de_reg.rs1
    )
  )

  ex_rs2 := MuxCase(
    de_reg.rs2,
    IndexedSeq(
      (forwardingUnit.io.forward_exe_rs2 === ForwardExeOperand.FWD_EW) -> ew_reg.alu,
      (forwardingUnit.io.forward_exe_rs2 === ForwardExeOperand.FWD_NONE) -> de_reg.rs2
    )
  )

  // ALU operations
  alu.io.A := ex_alu_op1
  alu.io.B := ex_alu_op2

  alu.io.alu_op := de_reg.ctrl.alu_op

  // Branch condition calc
  brCond.io.rs1 := ex_rs1
  brCond.io.rs2 := ex_rs2
  brCond.io.br_type := de_reg.ctrl.br_type

  // Pipelining

  when(reset.asBool || !full_stall && csr.io.exception) {
    pc_check := false.B
    illegal := false.B

  }.elsewhen(!full_stall && !csr.io.exception) {
    ew_reg.pc := de_reg.pc
    ew_reg.inst := de_reg.inst
    ew_reg.ctrl := de_reg.ctrl
    ew_reg.rs2 := de_reg.rs2
    ew_reg.alu := alu.io.out

    illegal := de_reg.ctrl.illegal

    // ew_reg.csr_in := Mux(de_reg.ctrl.imm_sel === ImmSel.IMM_Z, de_reg.immOut, de_reg.op1)
    ew_reg.csr_in := alu.io.out

    // Might need to convert this to a wire and make it a MuxLookup
    pc_check := de_reg.ctrl.pc_sel === PCSel.PC_ALU
  }

  forwardingUnit.io.ew_reg := ew_reg

  forwardingUnit.io.wb_rd := wb_rd_addr
  forwardingUnit.io.wb_en := ew_reg.ctrl.wb_en
  forwardingUnit.io.wb_sel := ew_reg.ctrl.wb_sel

  // Load
  val loffset = (ew_reg.alu(1) << 4.U).asUInt | (ew_reg.alu(0) << 3.U).asUInt
  val lshift = io.dcache.resp.bits.data >> loffset
  val load = MuxLookup(
    ew_reg.ctrl.ld_type.asUInt,
    io.dcache.resp.bits.data.zext,
    Seq(
      LdType.LD_LH.asUInt -> lshift(15, 0).asSInt,
      LdType.LD_LB.asUInt -> lshift(7, 0).asSInt,
      LdType.LD_LHU.asUInt -> lshift(15, 0).zext,
      LdType.LD_LBU.asUInt -> lshift(7, 0).zext
    )
  )

  // CSR access
  csr.io.stall := full_stall
  csr.io.in := ew_reg.csr_in
  csr.io.cmd := ew_reg.ctrl.csr_cmd
  csr.io.inst := ew_reg.inst
  csr.io.pc := ew_reg.pc
  csr.io.addr := ew_reg.alu
  csr.io.illegal := illegal
  csr.io.pc_check := pc_check
  csr.io.ld_type := ew_reg.ctrl.ld_type
  csr.io.st_type := ew_reg.ctrl.st_type
  io.host <> csr.io.host

  /**
    * Mem/Writeback stage
    */

  // D$ access
  val daddr = Mux(full_stall, ew_reg.alu, alu.io.sum) >> 2.U << 2.U
  val woffset = (alu.io.sum(1) << 4.U).asUInt | (alu.io.sum(0) << 3.U).asUInt

  // Note the request being made when the instruction is in the previous stage
  io.dcache.req.valid := !full_stall && (de_reg.ctrl.st_type.asUInt.orR || de_reg.ctrl.ld_type.asUInt.orR)
  io.dcache.req.bits.addr := daddr
  io.dcache.req.bits.data := ex_rs2 << woffset
  io.dcache.req.bits.mask := MuxLookup(
    Mux(full_stall, ew_reg.ctrl.st_type, de_reg.ctrl.st_type).asUInt,
    "b0000".U,
    Seq(
      StType.ST_SW.asUInt -> "b1111".U,
      StType.ST_SH.asUInt -> ("b11".U << alu.io.sum(1, 0)),
      StType.ST_SB.asUInt -> ("b1".U << alu.io.sum(1, 0))
    )
  )

  // Regfile Write
  val regWrite =
    MuxLookup(
      ew_reg.ctrl.wb_sel.asUInt,
      ew_reg.alu.zext,
      Seq(
        WbSel.WB_MEM.asUInt -> load,
        WbSel.WB_PC4.asUInt -> (ew_reg.pc + 4.U).zext,
        WbSel.WB_CSR.asUInt -> csr.io.out.zext
      )
    ).asUInt

  regFile.io.wen := ew_reg.ctrl.wb_en && !full_stall && !csr.io.exception
  regFile.io.waddr := wb_rd_addr
  regFile.io.wdata := regWrite

  // Abort store when there's an excpetion
  io.dcache.abort := csr.io.exception

  // TODO: re-enable through AOP
  if (conf.trace) {
    printf(
      "pc=[%x] W[r%d=%x][%d] Op1=[r%d][%x] Op2=[r%d][%x] inst=[%x] %c%c%c DASM(%x)\n",
      ew_reg.pc,
      wb_rd_addr,
      regWrite,
      ew_reg.ctrl.wb_en && !full_stall && !csr.io.exception,
      ew_reg.inst(RS1_MSB, RS1_LSB), // RS1 address
      RegNext(de_reg.op1),
      ew_reg.inst(RS2_MSB, RS2_LSB), // RS2 address
      RegNext(de_reg.op2),
      ew_reg.inst,
      Mux(io.ctrl.inst_kill, Str("K"), Str(" ")),
      MuxLookup(
        ew_reg.ctrl.pc_sel.asUInt,
        Str("?"),
        Seq(
          PCSel.PC_4.asUInt -> MuxCase(Str(" "), IndexedSeq((ew_reg.ctrl.br_type =/= BrType.BR_XXX) -> Str("B"))),
          PCSel.PC_ALU.asUInt -> Str("R"),
          PCSel.PC_EPC.asUInt -> Str("E")
        )
      ),
      Mux(csr.io.exception, Str("X"), Str(" ")),
      ew_reg.inst
    )
  }
}
