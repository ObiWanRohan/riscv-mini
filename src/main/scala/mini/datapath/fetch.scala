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

class FetchDecodePipelineRegister(xlen: Int) extends Bundle {
  // outputs to fetch/decode pipeline reg
  val pc = UInt(xlen.W)
  val inst = UInt(xlen.W)

  val ctrl = new ControlSignals
}

class FetchStageIO(xlen: Int) extends Bundle {
  val icache = Flipped(new CacheIO(xlen, xlen))

  // global control signals, logic moved to main datapath
  val full_stall = Input(Bool())
  val dec_stall = Input(Bool())
  val if_kill = Input(Bool())
  val dec_kill = Input(Bool())

  // Inputs used by the fetch stage from other stages
  val csr_epc = Input(UInt(xlen.W))
  val csr_evec = Input(UInt(xlen.W))
  val brCond_taken = Input(Bool())
  val alu_sum = Input(UInt(xlen.W))

  val fd_reg = Output(new FetchDecodePipelineRegister(xlen))
  val de_reg = Input(new DecodeExecutePipelineRegister(xlen))
//   add other IO ports to the fetch stage -- forwarding unit, ctrl, brCond, etc

}

class FetchStage(val conf: CoreConfig) extends Module {
  val io = IO(new FetchStageIO(conf.xlen))
  val started = RegNext(reset.asBool)

  io.brCond_taken := BrCondIO.brCond_taken
  io.alu_sum := AluIO.sum
  io.csr_epc := CSRIO.epc
  io.csr_evec := CSRIO.evec

  val fd_reg = RegInit(
    new FetchDecodePipelineRegister(conf.xlen)
      .Lit(
        _.pc -> 0.U,
        _.inst -> Instructions.NOP
      )
  )

  val pc = RegInit(Const.PC_START.U(conf.xlen.W) - 4.U(conf.xlen.W))
  // Next Program Counter
  val next_pc = MuxCase(
    pc + 4.U,
    IndexedSeq(
      (io.full_stall || io.dec_stall) -> pc,
      io.csr.exception -> io.csr_evec,
      (io.de_reg.ctrl.pc_sel === PCSel.PC_EPC) -> io.csr_epc,
      ((io.de_reg.ctrl.pc_sel === PCSel.PC_ALU) || (io.brCond_taken)) -> (io.alu_sum >> 1.U << 1.U),
      (io.de_reg.ctrl.pc_sel === PCSel.PC_0) -> pc
    )
  )

  val inst =
    Mux(started || io.csr.exception, Instructions.NOP, io.icache.resp.bits.data)

  pc := next_pc
  io.icache.req.bits.addr := next_pc
  io.icache.req.bits.data := 0.U
  io.icache.req.bits.mask := 0.U
  io.icache.req.valid := !io.full_stall
  io.icache.abort := false.B

  // Pipelining
  // Only update the instruction when not stalling
  when(!io.full_stall && !io.dec_stall) {
    fd_reg.pc := pc
    when(io.if_kill) {
      fd_reg.inst := Instructions.NOP
    }.otherwise {
      fd_reg.inst := inst
    }
  }
  // IO connections
  io.fd_reg := fd_reg
}
