// See LICENSE for license details.

package mini

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.testers._
// import chiseltest.simulator.{WriteWaveformAnnotation}
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

object DatapathTesterState extends ChiselEnum {
  val sInit, sRun = Value
}

class DatapathTester(datapath: => Datapath, testType: DatapathTest) extends BasicTester with TestUtils {
  val dut = Module(datapath)
  val ctrl = Module(new Control)
  val xlen = dut.conf.xlen

  dut.io.ctrl <> ctrl.io
  dut.io.host.fromhost.bits := DontCare
  dut.io.host.fromhost.valid := false.B

  override val insts = tests(testType)
  val maxTimeout = 100.U

  import DatapathTesterState._
  val state = RegInit(sInit)
  val (cntr, done) = Counter(state === sInit, insts.size)
  val timeout = RegInit(0.U(32.W))
  val mem = Mem(1 << 20, UInt(xlen.W))
  val iaddr = dut.io.icache.req.bits.addr / (xlen / 8).U
  val daddr = dut.io.dcache.req.bits.addr / (xlen / 8).U
  val write = (0 until (xlen / 8)).foldLeft(0.U) { (data, i) =>
    data |
      (Mux(dut.io.dcache.req.bits.mask(i), dut.io.dcache.req.bits.data, mem(daddr)) & (BigInt(0xff) << (8 * i)).U)
  }
  dut.reset := state === sInit
  dut.io.icache.resp.bits.data := RegNext(mem(iaddr))
  dut.io.icache.resp.valid := state === sRun
  dut.io.dcache.resp.bits.data := RegNext(mem(daddr))
  dut.io.dcache.resp.valid := state === sRun

  switch(state) {
    is(sInit) {
      (0 until Const.PC_START by 4).foreach { addr =>
        mem((addr / 4).U) := (if (addr == Const.PC_EVEC + (3 << 6)) fin else nop)
      }
      mem((Const.PC_START / (xlen / 8)).U + cntr) := VecInit(insts)(cntr)
      when(done) {
        printf(s"Instructions for ${testType}:\n")
        insts.zipWithIndex.foreach {
          case (inst, index) =>
            printf(s"Instruction $index : %x\n", inst)
        }
        state := sRun
      }
    }
    is(sRun) {
      when(dut.io.icache.req.valid) {
        printf(s"INST[%x] => %x, iaddr: %x\n", dut.io.icache.req.bits.addr, mem(iaddr), iaddr)
      }
      when(dut.io.dcache.req.valid) {
        when(dut.io.dcache.req.bits.mask.orR) {
          mem(daddr) := write
          printf("MEM[%x] <= %x\n", dut.io.dcache.req.bits.addr, write)
          // printf("Stall : %d\n", dut.stall)
        }.otherwise {
          printf("MEM[%x] => %x\n", dut.io.dcache.req.bits.addr, mem(daddr))
        }
      }
      timeout := timeout + 1.U
      assert(timeout < maxTimeout)
      when(dut.io.host.tohost =/= 0.U) {
        assert(
          dut.io.host.tohost === testResults(testType).U,
          s"* tohost: %d != ${testResults(testType)} *",
          dut.io.host.tohost
        )
        stop()
      }
    }
  }
}

class DatapathTests extends AnyFlatSpec with ChiselScalatestTester {
  val p = MiniConfig()
  val runWithVerilator = true

  "Datapath" should "pass basic addition test with verilator" in {
    test(new DatapathTester(new Datapath(p.core), BasicAdditionTest))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))
      .runUntilStop()
  }

  Seq(BypassTest, ExceptionTest).foreach { tst =>
    if (!runWithVerilator) {
      "Datapath" should s"pass $tst" in {
        // printf("Instructions for %s: \n %x", tst, insts.map(inst => Hexadecimal(inst)))
        test(new DatapathTester(new Datapath(p.core), tst)).runUntilStop()
      }
    } else {
      "Datapath" should s"pass $tst with verilator" in {
        test(new DatapathTester(new Datapath(p.core), tst))
          .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))
          .runUntilStop()
      }
    }
  }
}
