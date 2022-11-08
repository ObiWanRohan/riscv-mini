// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util.Valid

case class CoreConfig(
  trace:              Boolean = false,
  traceStack:         Boolean = false,
  xlen:               Int,
  makeAlu:            Int => Alu = new AluStage(_),
  makeBrCond:         Int => BrCond = new BrCondStage(_),
  makeImmGen:         Int => ImmGen = new ImmGenWire(_),
  makeForwardingUnit: Int => ForwardingUnit = new ForwardingUnit(_))

class HostIO(xlen: Int) extends Bundle {
  val fromhost = Flipped(Valid(UInt(xlen.W)))
  val tohost = Output(UInt(xlen.W))
}

class CoreIO(xlen: Int) extends Bundle {
  val host = new HostIO(xlen)
  val icache = Flipped(new CacheIO(xlen, xlen))
  val dcache = Flipped(new CacheIO(xlen, xlen))
}

class Core(val conf: CoreConfig) extends Module {
  val io = IO(new CoreIO(conf.xlen))
  val dpath = Module(new Datapath(conf))
  val ctrl = Module(new Control)

  io.host <> dpath.io.host
  dpath.io.icache <> io.icache
  dpath.io.dcache <> io.dcache
  dpath.io.ctrl <> ctrl.io
}
