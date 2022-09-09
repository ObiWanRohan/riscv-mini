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

class FetchDecodePipelineRegister(xlen: Int) extends Bundle {
  // outputs to fetch/decode pipeline reg
  val pc = UInt(xlen.W)
  val inst = UInt(xlen.W)

  val ctrl = new ControlSignals
}

class FetchStageIO(xlen: Int) extends Bundle {
  val icache = Flipped(new CacheIO(xlen, xlen))
  val full_stall = Output(Bool())
  val dec_stall = Output(Bool())
  val if_kill = Output(Bool())
  val dec_kill = Output(Bool())
  val fd_reg = Output(
    RegInit(
      new FetchDecodePipelineRegister(xlen).Lit(
        _.pc -> 0.U,
        _.inst -> Instructions.NOP
      )
    )
  )
//   add other IO ports to the fetch stage -- forwarding unit, ctrl, brCond, etc

}

class FetchStage(val conf: CoreConfig) extends Module {
  val io = IO(new FetchStageIO(xlen))
  val started = RegNext(reset.asBool)

  val fd_reg = io.fd_reg
  val de_reg = DecodeStage.io.de_reg

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
    io.fd_reg.pc := pc
    when(if_kill) {
      fd_reg.inst := Instructions.NOP
    }.otherwise {
      io.fd_reg.inst := inst
    }
  }

  forwardingUnit.io.fd_reg := fd_reg

}
