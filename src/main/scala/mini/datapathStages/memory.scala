package mini.DatapathStages

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import mini.common._
import mini.common.RISCVConstants._
import mini.{Alu, AluSel, CSR, CSRIOOutput, Cache, CacheIO, Const, ControlSignals, CoreConfig, HostIO, Instructions}
import mini.Control.{N, Y}

import CPUControlSignalTypes._
import mini.ForwardDecOperand._

class MemoryWritebackPipelineRegister(xlen: Int, numWays: Int) extends Bundle {
  val inst = chiselTypeOf(Instructions.NOP)
  val pc = SplitUInt(xlen, numWays)
  val wb_data = SplitUInt(xlen, numWays) //needed for writeback
  val rs2 = SplitUInt(xlen, numWays) // needed for writeback
  val dcache_out = SInt(xlen.W) //

  val ctrl = new ControlSignals
}

class MemoryStageIO(conf: CoreConfig) extends Bundle {
  val dcache = Flipped(new CacheIO(conf.xlen, conf.xlen))

  val full_stall = Input(Bool())
  val mem_stage_stall = Input(Bool())
  val de_reg = Input(new DecodeExecutePipelineRegister(conf.xlen, conf.numWays))
  val em_reg = Input(new ExecuteMemoryPipelineRegister(conf.xlen, conf.numWays))

  val alu = Input(new Bundle {
    val sum = SplitUInt(conf.xlen, conf.numWays)
  })
  val brCond = Input(new Bundle {
    val taken = Bool()
  })

  val illegal = Output(Bool())

  val host = (new HostIO(conf.xlen))
  val csr = Output(new CSRIOOutput(conf.xlen))
  val mw_reg = Output(new MemoryWritebackPipelineRegister(conf.xlen, conf.numWays))
}

class MemoryStage(val conf: CoreConfig) extends Module {
  val io = IO(new MemoryStageIO(conf))
  val csr = Module(new CSR(conf.xlen)) //mem stage

  val mw_reg = RegInit(
    (new MemoryWritebackPipelineRegister(conf.xlen, conf.numWays)).Lit(
      _.inst -> Instructions.NOP,
      // _.pc -> 0.U,
      // _.wb_data -> 0.U,
      // _.rs2 -> 0.U,
      _.dcache_out -> 0.S,
      _.ctrl -> ControlSignals.defaultSignals()
    )
  )

  val pc_check = RegInit(false.B)
  val illegal = RegInit(false.B)

  val regWrite = Wire(UInt(conf.xlen.W))
  val load = Wire(SInt(conf.xlen.W))

//   Load
  val loffset = if (conf.xlen / conf.numWays == 1) {
    (io.em_reg.alu(1) << 4.U).asUInt | (io.em_reg.alu(0) << 3.U).asUInt
  } else {
    (io.em_reg.alu(0)(1) << 4.U).asUInt | (io.em_reg.alu(0)(0) << 3.U).asUInt
  }

  val lshift = io.dcache.resp.bits.data >> loffset
  load := Mux(
    io.em_reg.ctrl.ld_type.asUInt.orR && (io.em_reg.alu === Const.FROMHOST_ADDR.U) && io.host.fromhost.valid,
    io.host.fromhost.bits.asSInt,
    MuxLookup(
      io.em_reg.ctrl.ld_type.asUInt,
      io.dcache.resp.bits.data.zext,
      Seq(
        LdType.LD_LH.asUInt -> lshift(15, 0).asSInt,
        LdType.LD_LB.asUInt -> lshift(7, 0).asSInt,
        LdType.LD_LHU.asUInt -> lshift(15, 0).zext,
        LdType.LD_LBU.asUInt -> lshift(7, 0).zext
      )
    )
  )
  // CSR access -----------------------------VERIFY CSR EXECUTE / MEMORY PIPELINE ACCESS BELOW
  csr.io.stall := io.full_stall
  csr.io.in := io.em_reg.csr_in.flatInterface
  csr.io.cmd := io.em_reg.ctrl.csr_cmd
  csr.io.inst := io.em_reg.inst
  csr.io.pc := io.em_reg.pc.flatInterface
  csr.io.addr := io.em_reg.alu.flatInterface
  csr.io.illegal := illegal
  csr.io.pc_check := pc_check
  csr.io.ld_type := io.em_reg.ctrl.ld_type
  csr.io.st_type := io.em_reg.ctrl.st_type

  // Temporarily changed for tests
  // io.host <> csr.io.host
  csr.io.host.fromhost.bits := 0.U
  csr.io.host.fromhost.valid := false.B

  val memReqValid =
    !io.full_stall && (io.em_reg.ctrl.st_type.asUInt.orR || io.em_reg.ctrl.ld_type.asUInt.orR)

  // D$ access
  // Setting the lower 2 bits to 0
  val daddr = Cat(io.em_reg.alu.asUInt(conf.xlen - 1, 2), 0.U(2.W))
  val woffset = if (conf.xlen / conf.numWays == 1) {
    (io.em_reg.alu(1) << 4.U).asUInt | (io.em_reg.alu(0) << 3.U).asUInt
  } else {
    (io.em_reg.alu(0)(1) << 4.U).asUInt | (io.em_reg.alu(0)(0) << 3.U).asUInt
  }

  val tohost_reg = Reg(UInt(conf.xlen.W))
  val tohost_mem_req = memReqValid && io.em_reg.ctrl.st_type.asUInt.orR && (daddr === Const.TOHOST_ADDR.U)

  // TODO : Add shifter here
  val storeMaskControlBits = if (conf.xlen / conf.numWays == 1) {
    MuxLookup(
      io.em_reg.ctrl.st_type.asUInt,
      "b0000".U,
      Seq(
        StType.ST_SW.asUInt -> "b1111".U,
        StType.ST_SH.asUInt -> ("b11".U << (io.em_reg.alu(1) << 1.U(1.W) | io.em_reg.alu(0))),
        StType.ST_SB.asUInt -> ("b1".U << (io.em_reg.alu(1) << 1.U(1.W) | io.em_reg.alu(0)))
      )
    )
  } else {
    MuxLookup(
      io.em_reg.ctrl.st_type.asUInt,
      "b0000".U,
      Seq(
        StType.ST_SW.asUInt -> "b1111".U,
        StType.ST_SH.asUInt -> ("b11".U << io.em_reg.alu(0)(1, 0)),
        StType.ST_SB.asUInt -> ("b1".U << io.em_reg.alu(0)(1, 0))
      )
    )

  }

  // Creating the complete mask from the store mask bits
  // Number of bytes in the data (as each mask bit controls a byte)
  val numMaskRepeat = conf.xlen / 8
  val storeMask = Wire(UInt(conf.xlen.W))
  // Converting the small mask to bits, repeating each element numMaskRepeat times,
  // reversing the output (as the flatmap starts at the first element), and concatenating all the bits
  storeMask := Cat(storeMaskControlBits.asBools.flatMap(Seq.fill(numMaskRepeat)(_)).reverse)

  // Writing to host IO
  when(tohost_mem_req) {
    tohost_reg := (io.em_reg.rs2 & storeMask).asUInt
  }

  val mem_load_request_sent = RegEnable(
    io.dcache.req.valid && io.em_reg.ctrl.ld_type.asUInt.orR,
    false.B,
    !io.full_stall
  )

  io.dcache.req.valid := (
    !io.full_stall
  ) && (
    io.em_reg.ctrl.st_type.asUInt.orR || io.em_reg.ctrl.ld_type.asUInt.orR
  ) && (
    !mem_load_request_sent
    // !RegNext(io.mem_stage_stall)
  )
  io.dcache.req.bits.addr := daddr
  io.dcache.req.bits.data := (io.em_reg.rs2 << woffset).flatInterface
  io.dcache.req.bits.mask := storeMaskControlBits
  // Abort store when there's an exception
  io.dcache.abort := csr.io.exception

  // forwardingUnit.io.mw_reg := mw_reg

  // Regfile Write data
  regWrite := MuxLookup(
    io.em_reg.ctrl.wb_sel.asUInt,
    io.em_reg.alu.asUInt.zext,
    Seq(
      WbSel.WB_MEM.asUInt -> load,
      WbSel.WB_PC4.asUInt -> (io.em_reg.pc + 4.U).asUInt.zext,
      WbSel.WB_CSR.asUInt -> csr.io.out.zext
    )
  ).asUInt

  // add pipeline stage -- ALUOut, Inst, ctrl
  // Pipelining
  when(reset.asBool || !io.full_stall && csr.io.exception) {
    pc_check := false.B
    illegal := false.B

  }.elsewhen(!io.full_stall && !csr.io.exception && !io.mem_stage_stall) {
    mw_reg.pc := io.em_reg.pc
    mw_reg.inst := io.em_reg.inst
    mw_reg.ctrl := io.em_reg.ctrl
    mw_reg.rs2 := io.em_reg.rs2

    mw_reg.wb_data := regWrite

    mw_reg.dcache_out := load

    io.csr := csr.io
    // io.em_reg.csr_in := alu.io.out

    illegal := io.de_reg.ctrl.illegal

    // Might need to convert this to a wire and make it a MuxLookup
    pc_check := (io.de_reg.ctrl.pc_sel === PCSel.PC_ALU) || io.brCond.taken
  }

  // Use data from memory if the memory request was valid in the previous cycle
  // i.e the data is being written in the current cycle. Otherwise send data from CSR
  io.host.tohost := Mux(RegNext(tohost_mem_req), tohost_reg, csr.io.host.tohost)

  io.mw_reg := mw_reg
  io.illegal := illegal
  io.csr := csr.io

}
