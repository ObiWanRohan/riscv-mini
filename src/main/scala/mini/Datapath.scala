// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import mini.common._
import mini.common.RISCVConstants._
import mini.DatapathStages._

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

  /** **** Control signals ****
    */

  // // Full pipeline stall for when the IMem or DMem is not responding
  // // This can also be called cache_miss_stall
  val full_stall = !io.icache.resp.valid || !io.dcache.resp.valid

  val de_reg = decodeStage.io.de_reg
  val de_ctrl_ld = decodeStage.io.de_reg.ctrl.ld_type
  val de_inst = decodeStage.io.de_reg.inst
  val fd_inst = fetchStage.io.fd_reg.inst

  // Decode Stage Stall
  // This would stop the Fetch and Decode stages (keep the same instruction) inside.
  // NOPs will be inserted in the Execute stage while this is asserted
  // Can also be called hazard_stall
  val dec_stall = Wire(Bool())

  // format: off
  dec_stall := (
    // Load Hazard
    // When a Load instruction is in the Execute stage
    // and the instruction in the Decode stage is trying to access the loaded value
    de_ctrl_ld =/= LdType.LD_XXX // Instruction in Execute stage is a load
      && (
        (
          de_inst(RD_MSB, RD_LSB) === fd_inst(RS1_MSB, RS1_LSB)
        ) || (
          de_inst(RD_MSB, RD_LSB) === fd_inst(RS2_MSB, RS2_LSB)
        )
      ) // And instruction in decode stage is reading the loaded value
  ) || (
    // Instruction being executed is a CSR instruction
    // Stalling here because the CSR execution happens in MEM stage and the next instruction might use the value
    (
      // CSR Command is one of W, S or C
      de_reg.ctrl.csr_cmd(0) || de_reg.ctrl.csr_cmd(1)
    ) && (
      (
        // Next instruction is a (M|S|U)RET
        io.ctrl.pc_sel === PCSel.PC_EPC
        ) || (
        // Next instruction is reading the destination register
        de_inst(RD_MSB, RD_LSB).orR
        && (
            de_inst(RD_MSB, RD_LSB) === fd_inst(RS1_MSB, RS1_LSB)
            || de_inst(RD_MSB, RD_LSB) === fd_inst(RS2_MSB, RS2_LSB)
          )
        )
      )
  )
  // || io.ctrl.fencei || RegNext(io.ctrl.fencei)
  // format: on

  // Kill Fetch stage
  // This means the instruction in the fetch stage will not pass to the decode stage
  // and a NOP will be inserted instead
  val if_kill = (
    (decodeStage.io.de_reg.ctrl.pc_sel =/= PCSel.PC_4)
  ) || (
    memoryStage.io.csr.exception
  ) || (
    executeStage.io.brCond.taken
  ) || (
    fetchStage.io.started
  )

  // Kill Decode stage
  // This means the instruction in the decode stage will not pass to the execute stage
  // and a NOP will be inserted instead
  val dec_kill = (
    decodeStage.io.de_reg.ctrl.pc_sel =/= PCSel.PC_4
  ) || (
    memoryStage.io.csr.exception
  ) || (
    memoryStage.io.illegal
  ) || (
    executeStage.io.brCond.taken
  )

  // global signal inputs to fetch stage
  fetchStage.io.full_stall := full_stall
  fetchStage.io.dec_stall := dec_stall
  fetchStage.io.if_kill := if_kill
  fetchStage.io.dec_kill := dec_kill
  // Inputs from othe stages
  fetchStage.io.csr := memoryStage.io.csr
  fetchStage.io.brCond := executeStage.io.brCond
  fetchStage.io.alu := executeStage.io.alu

  fetchStage.io.de_reg := decodeStage.io.de_reg
  io.icache <> fetchStage.io.icache

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
  decodeStage.io.writeback := writebackStage.io.writeback

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
  memoryStage.io.brCond := executeStage.io.brCond

  io.dcache <> memoryStage.io.dcache

  io.host <> memoryStage.io.host

  // writeback stage IO connections
  writebackStage.io.mw_reg := memoryStage.io.mw_reg

  forwardingUnit.io.fd_reg := decodeStage.io.fd_reg
  forwardingUnit.io.de_reg := decodeStage.io.de_reg
  forwardingUnit.io.em_reg := executeStage.io.em_reg
  forwardingUnit.io.mw_reg := memoryStage.io.mw_reg
  forwardingUnit.io.writeback := writebackStage.io.writeback

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
