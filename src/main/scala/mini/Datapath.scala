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
class Datapath(val conf: CoreConfig) extends Module {
  val io = IO(new DatapathIO(conf.xlen))
  // val csr = Module(new CSR(conf.xlen)) //mem stage
  // val regFile = Module(new RegFile(conf.xlen)) //decode stage
  // val alu = Module(conf.makeAlu(conf.xlen)) //execute stage
  // val immGen = Module(conf.makeImmGen(conf.xlen)) //decode stage
  // val brCond = Module(conf.makeBrCond(conf.xlen)) //execute stage
  val forwardingUnit = Module(conf.makeForwardingUnit(conf.xlen)) //datapath

  import Control._
  import CPUControlSignalTypes._
  import ForwardDecOperand._

  // Instanciating stage modules

  val fetchStage = Module(new FetchStage(conf))
  val decodeStage = Module(new DecodeStage(conf))
  val executeStage = Module(new ExecuteStage(conf))
  val memoryStage = Module(new MemoryStage(conf))
  val writebackStage = Module(new WritebackStage(conf))

  // control is part of core & takes input below
  io.ctrl.inst := fetchStage.io.fd_reg.inst

  // global signal inputs to fetch stage
  fetchStage.io.full_stall := full_stall
  fetchStage.io.dec_stall := dec_stall
  fetchStage.io.if_kill := if_kill
  fetchStage.io.dec_kill := dec_kill
  // Inputs from othe stages
  fetchStage.io.csr := memoryStage.csr.io
  fetchStage.io.brCond := executeStage.io.brCond
  fetchStage.io.alu := executeStage.io.alu

  fetchStage.io.de_reg := decodeStage.io.de_reg

  // decode stage IO connections
  // global signal inputs to decode stage
  decodeStage.io.full_stall := full_stall
  decodeStage.io.dec_stall := dec_stall
  decodeStage.io.if_kill := if_kill
  decodeStage.io.dec_kill := dec_kill
  // Input from control unit
  decodeStage.io.ctrl := io.ctrl

  decodeStage.io.brCond := executeStage.io.brCond
  decodeStage.io.csr := memoryStage.io.csr

  decodeStage.io.fd_reg := fetchStage.io.fd_reg
  decodeStage.io.em_reg := executeStage.io.em_reg
  decodeStage.io.mw_reg := memoryStage.io.mw_reg
  decodeStage.io.forwardSignals := forwardingUnit.io.forwardSignals

  // Execute stage IO connections
  executeStage.io.full_stall := full_stall
  executeStage.io.de_reg := decodeStage.io.de_reg
  executeStage.io.mw_reg := memoryStage.io.mw_reg

  executeStage.io.csr := memoryStage.io.csr
  executeStage.io.forwardSignals := forwardingUnit.io.forwardSignals

  // memory stage IO connections
  memoryStage.io.full_stall := full_stall
  memoryStage.io.de_reg := decodeStage.io.de_reg
  memoryStage.io.em_reg := executeStage.io.em_reg

  memoryStage.io.alu := executeStage.io.alu
  memoryStage.io.ex_rs2 := executeStage.io.ex_rs2
  memoryStage.io.pc_check := executeStage.io.pc_check
  memoryStage.io.illegal := executeStage.io.illegal

  io.host := memoryStage.io.host

  // writeback stage IO connections
  writebackStage.io.mw_reg := memoryStage.io.mw_reg

  forwardingUnit.io.fd_reg := decodeStage.io.fd_reg
  forwardingUnit.io.de_reg := decodeStage.io.de_reg
  forwardingUnit.io.em_reg := executeStage.io.em_reg
  forwardingUnit.io.mw_reg := memoryStage.io.mw_reg

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

  val de_ctrl_ld = decodeStage.io.de_reg.ctrl.ld_type
  val de_inst = decodeStage.io.de_reg.inst
  val fd_inst = fetchStage.io.fd_reg.inst

  // Decode Stage Stall
  // Can also be called hazard_stall
  val dec_stall = Wire(Bool())

  // Currently only stalling on load hazards
  dec_stall := (
    de_ctrl_ld =/= LdType.LD_XXX // Instruction in Execute stage is a load
      && (
        (de_inst(RD_MSB, RD_LSB) === fd_inst(RS1_MSB, RS1_LSB))
          || (de_inst(RD_MSB, RD_LSB) === fd_inst(RS2_MSB, RS2_LSB))
      ) // And instruction in decode stage is reading the loaded value
  )

  // Kill Fetch stage
  // This means the instruction in the fetch stage will not pass to the decode stage
  // and a NOP will be inserted instead
  val if_kill = (
    (decodeStage.io.de_reg.ctrl.pc_sel =/= PCSel.PC_4)
  ) || (
    memoryStage.csr.io.exception
  ) || (
    executeStage.brCond.io.taken
  ) // || cs_fencei || RegNext(cs_fencei) // Will add later

  // Kill Decode stage
  // This means the instruction in the decode stage will not pass to the execute stage
  // and a NOP will be inserted instead
  val dec_kill = (
    decodeStage.io.de_reg.ctrl.pc_sel =/= PCSel.PC_4
  ) || (
    memoryStage.csr.io.exception
  ) || (
    illegal
  ) || (
    executeStage.brCond.io.taken
  )

  /**
    * Memory stage
    */

  // Load
  // val loffset = (em_reg.alu(1) << 4.U).asUInt | (em_reg.alu(0) << 3.U).asUInt
  // val lshift = io.dcache.resp.bits.data >> loffset
  // load := MuxLookup(
  //   em_reg.ctrl.ld_type.asUInt,
  //   io.dcache.resp.bits.data.zext,
  //   Seq(
  //     LdType.LD_LH.asUInt -> lshift(15, 0).asSInt,
  //     LdType.LD_LB.asUInt -> lshift(7, 0).asSInt,
  //     LdType.LD_LHU.asUInt -> lshift(15, 0).zext,
  //     LdType.LD_LBU.asUInt -> lshift(7, 0).zext
  //   )
  // )

  // // CSR access -----------------------------VERIFY CSR EXECUTE / MEMORY PIPELINE ACCESS BELOW
  // csr.io.stall := fetchStage.io.full_stall
  // csr.io.in := em_reg.csr_in
  // csr.io.cmd := em_reg.ctrl.csr_cmd
  // csr.io.inst := em_reg.inst
  // csr.io.pc := em_reg.pc
  // csr.io.addr := em_reg.alu
  // csr.io.illegal := illegal
  // csr.io.pc_check := pc_check
  // csr.io.ld_type := em_reg.ctrl.ld_type
  // csr.io.st_type := em_reg.ctrl.st_type

  // // Temporarily changed for tests
  // // io.host <> csr.io.host
  // csr.io.host.fromhost.bits := 0.U
  // csr.io.host.fromhost.valid := false.B

  // val memReqValid =
  //   !full_stall && (decodeStage.io.de_reg.ctrl.st_type.asUInt.orR || decodeStage.io.de_reg.ctrl.ld_type.asUInt.orR)

  // val wb_rd_addr = mw_reg.inst(RD_MSB, RD_LSB)

  // val tohost_reg = Reg(UInt(conf.xlen.W))

  // val tohost_mem_req = memReqValid && de_reg.ctrl.st_type.asUInt.orR && daddr === Const.HOST_ADDR.U

  // Use data from memory if the memory request was valid in the previous cycle
  // i.e the data is being written in the current cycle. Otherwise send data from CSR
  // io.host.tohost := Mux(RegNext(tohost_mem_req), tohost_reg, csr.io.host.tohost)

  // Writing to host IO
  // when(tohost_mem_req) {
  //   // TODO, add mask here
  //   tohost_reg := ex_rs2
  // }

  // // Note the request being made when the instruction is in the previous stage
  // io.dcache.req.valid := !full_stall && (de_reg.ctrl.st_type.asUInt.orR || de_reg.ctrl.ld_type.asUInt.orR)
  // io.dcache.req.bits.addr := daddr
  // io.dcache.req.bits.data := ex_rs2 << woffset
  // io.dcache.req.bits.mask := MuxLookup(
  //   Mux(fetchStage.io.full_stall, em_reg.ctrl.st_type, de_reg.ctrl.st_type).asUInt,
  //   "b0000".U,
  //   Seq(
  //     StType.ST_SW.asUInt -> "b1111".U,
  //     StType.ST_SH.asUInt -> ("b11".U << alu.io.sum(1, 0)),
  //     StType.ST_SB.asUInt -> ("b1".U << alu.io.sum(1, 0))
  //   )
  // )
  // Abort store when there's an excpetion
  // io.dcache.abort := csr.io.exception

  // forwardingUnit.io.mw_reg := mw_reg

  // // Regfile Write data
  // regWrite := MuxLookup(
  //   em_reg.ctrl.wb_sel.asUInt,
  //   em_reg.alu.zext,
  //   Seq(
  //     WbSel.WB_MEM.asUInt -> load,
  //     WbSel.WB_PC4.asUInt -> (em_reg.pc + 4.U).zext,
  //     WbSel.WB_CSR.asUInt -> csr.io.out.zext
  //   )
  // ).asUInt

  // add pipeline stage -- ALUOut, Inst, ctrl
  // Pipelining
  // when(reset.asBool || !fetchStage.io.full_stall && csr.io.exception) {
  //   pc_check := false.B
  //   illegal := false.B

  // }.elsewhen(!fetchStage.io.full_stall && !csr.io.exception) {
  //   mw_reg.pc := em_reg.pc
  //   mw_reg.inst := em_reg.inst
  //   mw_reg.ctrl := em_reg.ctrl
  //   mw_reg.rs2 := em_reg.rs2

  //   mw_reg.wb_data := regWrite

  //   mw_reg.dcache_out := load
  // em_reg.csr_in := alu.io.out

  //   // illegal := de_reg.ctrl.illegal

  //   // Might need to convert this to a wire and make it a MuxLookup
  //   // pc_check := de_reg.ctrl.pc_sel === PCSel.PC_ALU

  // }

  // TODO: re-enable through AOP
  // if (conf.trace) {
  //   printf(
  //     "pc=[%x] W[r%d=%x][%d] OpA=[r%d][%x] OpB=[r%d][%x] inst=[%x] %c%c%c DASM(%x)\n",
  //     em_reg.pc,
  //     wb_rd_addr,
  //     regWrite,
  //     em_reg.ctrl.wb_en && !full_stall && !csr.io.exception,
  //     em_reg.inst(RS1_MSB, RS1_LSB), // RS1 address
  //     RegNext(de_reg.opA),
  //     em_reg.inst(RS2_MSB, RS2_LSB), // RS2 address
  //     RegNext(de_reg.opB),
  //     em_reg.inst,
  //     Mux(io.ctrl.inst_kill, Str("K"), Str(" ")),
  //     MuxLookup(
  //       em_reg.ctrl.pc_sel.asUInt,
  //       Str("?"),
  //       Seq(
  //         PCSel.PC_4.asUInt -> MuxCase(Str(" "), IndexedSeq((em_reg.ctrl.br_type =/= BrType.BR_XXX) -> Str("B"))),
  //         PCSel.PC_ALU.asUInt -> Str("R"),
  //         PCSel.PC_EPC.asUInt -> Str("E")
  //       )
  //     ),
  //     Mux(csr.io.exception, Str("X"), Str(" ")),
  //     em_reg.inst
  //   )
  // }
}
