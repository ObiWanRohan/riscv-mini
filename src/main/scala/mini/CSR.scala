// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import CPUControlSignalTypes._
import common.RISCVConstants._

class MachineStatusCSR(xlen: Int) extends Bundle {
  val SD = UInt(1.W)
  val TSR = UInt(1.W)
  val TW = UInt(1.W)
  val TVM = UInt(1.W)
  val MXR = UInt(1.W)
  val SUM = UInt(1.W)
  val MPRV = UInt(1.W)
  val XS = UInt(2.W)
  val FS = UInt(2.W)
  val MPP = UInt(2.W)
  val SPP = UInt(1.W)
  val MPIE = UInt(1.W)
  val SPIE = UInt(1.W)
  val UPIE = UInt(1.W)
  val SIE = UInt(1.W)
  val UIE = UInt(1.W)
}

class MachineTrapVectorCSR(xlen: Int) extends Bundle {
  val base = UInt((xlen - 2).W)
  val mode = UInt(2.W)
}

class MachinePendingInterruptCSR(xlen: Int) extends Bundle {
  val MEIP = RegInit(false.B)
  val HEIP = false.B
  val SEIP = false.B
  val UEIP = false.B

  val MTIP = RegInit(false.B)
  val HTIP = false.B
  val STIP = false.B
  val UTIP = false.B

  val MSIP = RegInit(false.B)
  val HSIP = false.B
  val SSIP = false.B
  val USIP = false.B
}

class MachineInterruptEnableCSR(xlen: Int) extends {

  val MTIE = RegInit(false.B)
  val HTIE = false.B
  val STIE = false.B
  val UTIE = false.B

  val MSIE = RegInit(false.B)
  val HSIE = false.B
  val SSIE = false.B
  val USIE = false.B

  val MEIE = RegInit(false.B)
  val HEIE = false.B
  val SEIE = false.B
  val UEIE = false.B
}
object CSR {
  val N = 0.U(3.W)
  val W = 1.U(3.W)
  val S = 2.U(3.W)
  val C = 3.U(3.W)
  val P = 4.U(3.W)

  // Supports machine & user modes
  val PRV_U = 0x0.U(2.W)
  val PRV_M = 0x3.U(2.W)

  // User-level CSR addrs
  val cycle = 0xc00.U(CSR_ADDR_WIDTH)
  val time = 0xc01.U(CSR_ADDR_WIDTH)
  val instret = 0xc02.U(CSR_ADDR_WIDTH)
  val cycleh = 0xc80.U(CSR_ADDR_WIDTH)
  val timeh = 0xc81.U(CSR_ADDR_WIDTH)
  val instreth = 0xc82.U(CSR_ADDR_WIDTH)

  // Supervisor-level CSR addrs
  val stvec = 0x105.U(CSR_ADDR_WIDTH)
  val cyclew = 0x900.U(CSR_ADDR_WIDTH)
  val timew = 0x901.U(CSR_ADDR_WIDTH)
  val instretw = 0x902.U(CSR_ADDR_WIDTH)
  val cyclehw = 0x980.U(CSR_ADDR_WIDTH)
  val timehw = 0x981.U(CSR_ADDR_WIDTH)
  val instrethw = 0x982.U(CSR_ADDR_WIDTH)

  // Machine-level CSR addrs
  // Machine Information Registers
  val mvendorid = 0xf11.U(CSR_ADDR_WIDTH)
  val marchid = 0xf12.U(CSR_ADDR_WIDTH)
  val mimpid = 0xf13.U(CSR_ADDR_WIDTH)
  val mhartid = 0xf14.U(CSR_ADDR_WIDTH)

  // Machine Trap Setup
  val mstatus = 0x300.U(CSR_ADDR_WIDTH)
  val misa = 0x301.U(CSR_ADDR_WIDTH)
  val medeleg = 0x302.U(CSR_ADDR_WIDTH)
  val mideleg = 0x303.U(CSR_ADDR_WIDTH)
  val mie = 0x304.U(CSR_ADDR_WIDTH)
  val mtvec = 0x305.U(CSR_ADDR_WIDTH)
  val mcounteren = 0x306.U(CSR_ADDR_WIDTH)
  val mtimecmp = 0x321.U(CSR_ADDR_WIDTH)

  // Machine Timers and Counters
  val mtime = 0x701.U(CSR_ADDR_WIDTH)
  val mtimeh = 0x741.U(CSR_ADDR_WIDTH)
  val mcycle = 0xb00.U(CSR_ADDR_WIDTH)
  val mcycleh = 0xb80.U(CSR_ADDR_WIDTH)
  val minstret = 0xb02.U(CSR_ADDR_WIDTH)
  val minstreth = 0xb82.U(CSR_ADDR_WIDTH)

  // Machine Trap Handling
  val mscratch = 0x340.U(CSR_ADDR_WIDTH)
  val mepc = 0x341.U(CSR_ADDR_WIDTH)
  val mcause = 0x342.U(CSR_ADDR_WIDTH)
  val mtval = 0x343.U(CSR_ADDR_WIDTH)
  val mip = 0x344.U(CSR_ADDR_WIDTH)

  // Machine Memory Protection
  val pmpcfg0 = 0x3a0.U(CSR_ADDR_WIDTH)
  val pmpcfg1 = 0x3a1.U(CSR_ADDR_WIDTH)
  val pmpcfg2 = 0x3a2.U(CSR_ADDR_WIDTH)
  val pmpcfg3 = 0x3a3.U(CSR_ADDR_WIDTH)
  val pmpaddr0 = 0x3b0.U(CSR_ADDR_WIDTH)
  val pmpaddr1 = 0x3b1.U(CSR_ADDR_WIDTH)
  val pmpaddr2 = 0x3b2.U(CSR_ADDR_WIDTH)
  val pmpaddr3 = 0x3b3.U(CSR_ADDR_WIDTH)
  val pmpaddr4 = 0x3b4.U(CSR_ADDR_WIDTH)
  val pmpaddr5 = 0x3b5.U(CSR_ADDR_WIDTH)
  val pmpaddr6 = 0x3b6.U(CSR_ADDR_WIDTH)
  val pmpaddr7 = 0x3b7.U(CSR_ADDR_WIDTH)
  val pmpaddr8 = 0x3b8.U(CSR_ADDR_WIDTH)
  val pmpaddr9 = 0x3b9.U(CSR_ADDR_WIDTH)
  val pmpaddr10 = 0x3ba.U(CSR_ADDR_WIDTH)
  val pmpaddr11 = 0x3bb.U(CSR_ADDR_WIDTH)
  val pmpaddr12 = 0x3bc.U(CSR_ADDR_WIDTH)
  val pmpaddr13 = 0x3bd.U(CSR_ADDR_WIDTH)
  val pmpaddr14 = 0x3be.U(CSR_ADDR_WIDTH)
  val pmpaddr15 = 0x3bf.U(CSR_ADDR_WIDTH)

  // Machine HITF
  val mtohost = 0x780.U(CSR_ADDR_WIDTH)
  val mfromhost = 0x781.U(CSR_ADDR_WIDTH)

  val satp = 0x180.U(CSR_ADDR_WIDTH)

  val regs = List(
    cycle,
    time,
    instret,
    cycleh,
    timeh,
    instreth,
    stvec,
    cyclew,
    timew,
    instretw,
    cyclehw,
    timehw,
    instrethw,
    mvendorid,
    marchid,
    mimpid,
    mhartid,
    misa,
    mtvec,
    medeleg,
    mideleg,
    mie,
    mtimecmp,
    mcycle,
    mcycleh,
    minstret,
    minstreth,
    mtime,
    mtimeh,
    mscratch,
    mepc,
    mcause,
    mtval,
    mip,
    mtohost,
    mfromhost,
    mstatus,
    satp // TODO: Shift all valid register addresses to another file
  )
}

object Cause {
  val InstAddrMisaligned = 0x0.U
  val IllegalInst = 0x2.U
  val Breakpoint = 0x3.U
  val LoadAddrMisaligned = 0x4.U
  val StoreAddrMisaligned = 0x6.U
  val EcallFromUMode = 0x8.U
}

class CSRIOOutput(xlen: Int) extends Bundle {
  val epc = UInt(xlen.W)
  val evec = UInt(xlen.W)
  val exception = Bool()
}

object MISAMappings {
  val MXLEN_32 = 1.U(2.W)
  val MXLEN_64 = 2.U(2.W)
  val MXLEN_128 = 3.U(2.W)

  val A = (1 << ('A' - 'A')).U(26.W) // Atomic extension
  val C = (1 << ('C' - 'A')).U(26.W) // Compressed extension
  val D = (1 << ('D' - 'A')).U(26.W) // Double-precision floating-point extension
  val E = (1 << ('E' - 'A')).U(26.W) // RV32E base ISA
  val F = (1 << ('F' - 'A')).U(26.W) // Single-precision floating-point extension
  val G = (1 << ('G' - 'A')).U(26.W) // Additional standard extensions present
  val H = (1 << ('H' - 'A')).U(26.W) // Hypervisor extension
  val I = (1 << ('I' - 'A')).U(26.W) // RV32I/64I/128I base ISA
  val M = (1 << ('M' - 'A')).U(26.W) // Integer Multiply/Divide extension
  val N = (1 << ('N' - 'A')).U(26.W) // User-level interrupts supported
  val Q = (1 << ('Q' - 'A')).U(26.W) // Quad-precision floating-point extension
  val S = (1 << ('S' - 'A')).U(26.W) // Supervisor mode implemented
  val U = (1 << ('U' - 'A')).U(26.W) // User mode implemented
  val X = (1 << ('X' - 'A')).U(26.W) // Non-standard extensions present

}

class CSRIO(xlen: Int) extends Bundle {
  val stall = Input(Bool())
  val cmd = Input(UInt(3.W))
  val in = Input(UInt(xlen.W))
  val out = Output(UInt(xlen.W))
  // Excpetion
  val pc = Input(UInt(xlen.W))
  val addr = Input(UInt(xlen.W))
  val inst = Input(UInt(xlen.W))
  val illegal = Input(Bool())
  val st_type = Input(StType())
  val ld_type = Input(LdType())
  val pc_check = Input(Bool())
  val exception = Output(Bool())
  val evec = Output(UInt(xlen.W))
  val epc = Output(UInt(xlen.W))
  // HTIF
  val host = new HostIO(xlen)
}

class CSR(val xlen: Int) extends Module {

  val io = IO(new CSRIO(xlen))

  val csr_addr = io.inst(CSR_ADDR_MSB, CSR_ADDR_LSB)
  val rs1_addr = io.inst(RS1_MSB, RS1_LSB)

  // user counters
  val time = RegInit(0.U((COUNTER_WIDTH / 2).W))
  val timeh = RegInit(0.U((COUNTER_WIDTH / 2).W))
  val cycle = RegInit(0.U((COUNTER_WIDTH / 2).W))
  val cycleh = RegInit(0.U((COUNTER_WIDTH / 2).W))
  val instret = RegInit(0.U((COUNTER_WIDTH / 2).W))
  val instreth = RegInit(0.U((COUNTER_WIDTH / 2).W))
  val stvec = RegInit(0.U(xlen.W))

  val misa = Cat(
    MISAMappings.MXLEN_32,
    0.U((xlen - 28).W),
    MISAMappings.I |
      MISAMappings.U
  )

  val mvendorid = 0.U(xlen.W) // not implemented
  val marchid = 0.U(xlen.W) // not implemented
  val mimpid = 0.U(xlen.W) // not implemented
  val mhartid = 0.U(xlen.W) // only one hart

  // interrupt enable stack
  // Current Privilege mode
  val PRV = RegInit(CSR.PRV_M)

  // Previous Privilege Modes
  val MPP = RegInit(CSR.PRV_M)
  val HPP = 0.U(2.W)
  val SPP = 0.U(1.W)

  // Per-mode Global Interrupt Enable Bits
  val MIE = RegInit(false.B)
  val HIE = false.B
  val SIE = false.B
  val UIE = RegInit(false.B)

  // Per-mode Previous Interrupt Enable Bits
  val MPIE = RegInit(true.B)
  val SPIE = false.B
  val UPIE = RegInit(true.B)

  // Memory (Modify PRiVilege) bit
  val MPRV = RegInit(false.B)
  // Timeout Wait
  val TW = 0.U(1.U)

  /*
   * These bits are hardwired to 0 since S-mode is not supported
   */
  // (Make eXecutable Readable) bit
  val MXR = false.B
  // permit (Supervisor User Memory) access
  val SUM = false.B
  // virtualization management fields
  // Trap SRET
  val TSR = 0.U(1.W)
  // Trap Virtual Memory
  val TVM = 0.U(1.W)
  // extention context status
  // Floating point unit Status
  val FS = 0.U(2.W)
  // User mode extension Status
  val XS = 0.U(2.W)
  // SD shows the state of FS and XS (but we have it hardwired)
  val SD = false.B

  /*
   * Machine mode registers
   */

  // val _mstatus = RegInit(
  //   (new MachineStatusCSR).Lit(
  //     _.PRV -> CSR.PRV_M,
  //     _.MPP -> CSR.PRV_U,
  //     _.HPP -> 0.U(2.W),
  //     _.SPP -> 0.U(1.W),
  //     _.MIE -> false.B,
  //     _.HIE -> false.B,
  //     _.SIE -> false.B,
  //     _.UIE -> false.B,
  //     _.MPRV -> false.B,
  //     _.TW -> 0.U(1.U),
  //     _.MXR -> false.B,
  //     _.SUM -> false.B,
  //     _.TSR -> 0.U(1.W),
  //     _.TVM -> 0.U(1.W),
  //     _.FS -> 0.U(2.W),
  //     _.XS -> 0.U(2.W)
  //   )
  // )
  // val SD = (_mstatus.FS === 3.U) || (_mstatus.XS === 3.U)

  // Register view for output
  val mstatus = Cat(
    SD,
    0.U(8.W),
    TSR,
    TW,
    TVM,
    MXR,
    SUM,
    MPRV,
    XS,
    FS,
    MPP,
    0.U(2.W),
    SPP,
    MPIE,
    0.U(1.W),
    SPIE,
    UPIE,
    0.U(1.W),
    SIE,
    UIE
  )

  val _mtvec = RegInit(
    new MachineTrapVectorCSR(xlen).Lit(
      _.base -> Const.PC_EVEC.U,
      _.mode -> 0.U
    )
  )

  // Shifting the base by 2 as it is supposed to be aligned to 4 bytes
  val mtvec = Cat(_mtvec.base, _mtvec.mode)
  val medeleg = RegInit(0x0.U(xlen.W))
  val mideleg = RegInit(0x0.U(xlen.W))

  // Memory Protection
  val pmpcfg0 = 0x0.U(8.W)
  val pmpcfg1 = 0x0.U(8.W)
  val pmpcfg2 = 0x0.U(8.W)
  val pmpcfg3 = 0x0.U(8.W)
  val pmpaddr0 = 0x0.U(8.W)
  val pmpaddr1 = 0x0.U(8.W)
  val pmpaddr2 = 0x0.U(8.W)
  val pmpaddr3 = 0x0.U(8.W)
  val pmpaddr4 = 0x0.U(8.W)
  val pmpaddr5 = 0x0.U(8.W)
  val pmpaddr6 = 0x0.U(8.W)
  val pmpaddr7 = 0x0.U(8.W)
  val pmpaddr8 = 0x0.U(8.W)
  val pmpaddr9 = 0x0.U(8.W)
  val pmpaddr10 = 0x0.U(8.W)
  val pmpaddr11 = 0x0.U(8.W)
  val pmpaddr12 = 0x0.U(8.W)
  val pmpaddr13 = 0x0.U(8.W)
  val pmpaddr14 = 0x0.U(8.W)
  val pmpaddr15 = 0x0.U(8.W)

  // interrupt registers
  val MEIP = RegInit(false.B)
  val HEIP = false.B
  val SEIP = false.B
  val UEIP = false.B

  val MTIP = RegInit(false.B)
  val HTIP = false.B
  val STIP = false.B
  val UTIP = false.B

  val MSIP = RegInit(false.B)
  val HSIP = false.B
  val SSIP = false.B
  val USIP = false.B

  val MTIE = RegInit(false.B)
  val HTIE = false.B
  val STIE = false.B
  val UTIE = false.B

  val MSIE = RegInit(false.B)
  val HSIE = false.B
  val SSIE = false.B
  val USIE = false.B

  val MEIE = RegInit(false.B)
  val HEIE = false.B
  val SEIE = false.B
  val UEIE = false.B

  // val _mip = RegInit(new MachinePendingInterruptCSR(xlen))
  // val _mie = RegInit(new MachineInterruptEnableCSR(xlen))

  val mip = Cat(
    0.U((xlen - 12).W),
    MEIP,
    HEIP,
    SEIP,
    UEIP,
    MTIP,
    HTIP,
    STIP,
    UTIP,
    MSIP,
    HSIP,
    SSIP,
    USIP
  ).asUInt
  val mie = Cat(
    0.U((xlen - 12).W),
    MEIE,
    HEIE,
    SEIE,
    UEIE,
    MTIE,
    HTIE,
    STIE,
    UTIE,
    MSIE,
    HSIE,
    SSIE,
    USIE
  ).asUInt

  // Counter Enable for Lower modes
  val mcounteren = 0.U(32.W)

  val mtimecmp = Reg(UInt(xlen.W))

  val mscratch = Reg(UInt(xlen.W))
  val mepc = Reg(UInt(xlen.W))
  val mcause = Reg(UInt(xlen.W))
  val mtval = Reg(UInt(xlen.W))

  val mtohost = RegInit(0.U(xlen.W))
  val mfromhost = Reg(UInt(xlen.W))
  io.host.tohost := mtohost
  when(io.host.fromhost.valid) {
    mfromhost := io.host.fromhost.bits
  }

  val satp = 0.U(xlen.W)

  val csrFile = Seq(
    BitPat(CSR.cycle) -> cycle,
    BitPat(CSR.time) -> time,
    BitPat(CSR.instret) -> instret,
    BitPat(CSR.cycleh) -> cycleh,
    BitPat(CSR.timeh) -> timeh,
    BitPat(CSR.instreth) -> instreth,
    BitPat(CSR.stvec) -> stvec,
    BitPat(CSR.cyclew) -> cycle,
    BitPat(CSR.timew) -> time,
    BitPat(CSR.instretw) -> instret,
    BitPat(CSR.cyclehw) -> cycleh,
    BitPat(CSR.timehw) -> timeh,
    BitPat(CSR.instrethw) -> instreth,
    BitPat(CSR.mvendorid) -> mvendorid,
    BitPat(CSR.marchid) -> marchid,
    BitPat(CSR.mimpid) -> mimpid,
    BitPat(CSR.mhartid) -> mhartid,
    BitPat(CSR.misa) -> misa,
    BitPat(CSR.mtvec) -> mtvec,
    BitPat(CSR.medeleg) -> medeleg,
    BitPat(CSR.mideleg) -> mideleg,
    BitPat(CSR.mie) -> mie,
    BitPat(CSR.mtimecmp) -> mtimecmp,
    BitPat(CSR.mcycle) -> cycle,
    BitPat(CSR.mcycleh) -> cycleh,
    BitPat(CSR.minstret) -> instret,
    BitPat(CSR.minstreth) -> instreth,
    BitPat(CSR.mtime) -> time,
    BitPat(CSR.mtimeh) -> timeh,
    BitPat(CSR.mscratch) -> mscratch,
    BitPat(CSR.mepc) -> mepc,
    BitPat(CSR.mcause) -> mcause,
    BitPat(CSR.mtval) -> mtval,
    BitPat(CSR.mip) -> mip,
    BitPat(CSR.mtohost) -> mtohost,
    BitPat(CSR.mfromhost) -> mfromhost,
    BitPat(CSR.mstatus) -> mstatus,
    BitPat(CSR.mcounteren) -> mcounteren,
    BitPat(CSR.pmpcfg0) -> pmpcfg0,
    BitPat(CSR.pmpcfg1) -> pmpcfg1,
    BitPat(CSR.pmpcfg2) -> pmpcfg2,
    BitPat(CSR.pmpcfg3) -> pmpcfg3,
    BitPat(CSR.pmpaddr0) -> pmpaddr0,
    BitPat(CSR.pmpaddr1) -> pmpaddr1,
    BitPat(CSR.pmpaddr2) -> pmpaddr2,
    BitPat(CSR.pmpaddr3) -> pmpaddr3,
    BitPat(CSR.pmpaddr4) -> pmpaddr4,
    BitPat(CSR.pmpaddr5) -> pmpaddr5,
    BitPat(CSR.pmpaddr6) -> pmpaddr6,
    BitPat(CSR.pmpaddr7) -> pmpaddr7,
    BitPat(CSR.pmpaddr8) -> pmpaddr8,
    BitPat(CSR.pmpaddr9) -> pmpaddr9,
    BitPat(CSR.pmpaddr10) -> pmpaddr10,
    BitPat(CSR.pmpaddr11) -> pmpaddr11,
    BitPat(CSR.pmpaddr12) -> pmpaddr12,
    BitPat(CSR.pmpaddr13) -> pmpaddr13,
    BitPat(CSR.pmpaddr14) -> pmpaddr14,
    BitPat(CSR.pmpaddr15) -> pmpaddr15,
    BitPat(CSR.satp) -> satp
  )

  io.out := Lookup(csr_addr, 0.U, csrFile).asUInt

  val privValid = csr_addr(9, 8) <= PRV
  val privInst = io.cmd === CSR.P

  // Computed these conditions manually from instruction bit patterns
  val isEcall = privInst && !csr_addr(0) && !csr_addr(1) && !csr_addr(8) && !csr_addr(9)
  val isEbreak = privInst && csr_addr(0) && !csr_addr(1) && !csr_addr(8) && !csr_addr(9)

  val isMret = privInst && !csr_addr(0) && csr_addr(1) && csr_addr(8) && csr_addr(9)
  val isUret = privInst && !csr_addr(0) && csr_addr(1) && !csr_addr(8) && !csr_addr(9)

  val isSModeAddr = !csr_addr(9) && !csr_addr(8)

  val csrValid = csrFile.map(_._1 === csr_addr).reduce(_ || _) || isSModeAddr
  val csrRO = csr_addr(11, 10).andR

  val wen = io.cmd === CSR.W || io.cmd(1) && rs1_addr.orR
  val wdata = MuxLookup(
    io.cmd,
    0.U,
    Seq(
      CSR.W -> io.in,
      CSR.S -> (io.out | io.in),
      CSR.C -> (io.out & ~io.in)
    )
  )

  val iaddrInvalid = io.pc_check && io.addr(1)
  val laddrInvalid = MuxLookup(
    io.ld_type.asUInt,
    false.B,
    Seq(LdType.LD_LW.asUInt -> io.addr(1, 0).orR, LdType.LD_LH.asUInt -> io.addr(0), LdType.LD_LHU.asUInt -> io.addr(0))
  )
  val saddrInvalid = MuxLookup(
    io.st_type.asUInt,
    false.B,
    Seq(
      StType.ST_SW.asUInt -> io.addr(1, 0).orR,
      StType.ST_SH.asUInt -> io.addr(0)
    )
  )

  io.exception := (
    (
      io.illegal
    ) || (
      iaddrInvalid
    ) || (
      laddrInvalid
    ) || (
      saddrInvalid
    ) || (
      io.cmd(1, 0).orR && (!csrValid || !privValid)
    ) || (
      wen && csrRO
    ) || (
      privInst && !privValid
    ) || (
      isEcall
    ) || isEbreak
  )
  io.evec := mtvec
  io.epc := mepc

  // Counters
  time := time + 1.U
  when(time.andR) { timeh := timeh + 1.U }
  cycle := cycle + 1.U
  when(cycle.andR) { cycleh := cycleh + 1.U }

  val isInstRet = io.inst =/= Instructions.NOP && (!io.exception || isEcall || isEbreak) && !io.stall
  // Instructions counter
  when(isInstRet) { instret := instret + 1.U }
  when(isInstRet && instret.andR) { instreth := instreth + 1.U }

  when(!io.stall) {
    when(io.exception) {
      mepc := io.pc >> 2.U << 2.U
      mcause := MuxCase(
        Cause.IllegalInst,
        IndexedSeq(
          iaddrInvalid -> Cause.InstAddrMisaligned,
          laddrInvalid -> Cause.LoadAddrMisaligned,
          saddrInvalid -> Cause.StoreAddrMisaligned,
          (isEcall) -> Cause.EcallFromUMode,
          isEbreak -> Cause.Breakpoint
        )
      )

      // Move to M mode
      PRV := CSR.PRV_M
      // Disable User level interrupts
      UIE := false.B

      when(PRV === CSR.PRV_M) {
        MPP := CSR.PRV_M
        MPIE := MIE
      }.elsewhen(PRV === CSR.PRV_U) {
        MPP := CSR.PRV_U
        UPIE := UIE
      }

      when(laddrInvalid || saddrInvalid) {
        mtval := io.addr
      }.elsewhen(iaddrInvalid) {
        mtval := io.addr >> 1.U << 1.U
      }
    }.elsewhen(isMret) {
      PRV := MPP
      MIE := MPIE
      MPIE := 1.U
      MPP := CSR.PRV_U
    }.elsewhen(isUret) {
      // PRV := CSR.PRV_U
      // UIE := UPIE
      // UPIE := 1.U
    }.elsewhen(wen) {
      when(csr_addr === CSR.mstatus) {
        MIE := wdata(3)
        UIE := wdata(0)
        // Check if the previous mode bits can be written
        // MPIE := wdata(7)
        // UPIE := wdata(4)
      }
        .elsewhen(csr_addr === CSR.mip) {
          // TODO: Check privilege level before writing
          MEIP := wdata(11)
          MTIP := wdata(7)
          MSIP := wdata(3)
        }
        .elsewhen(csr_addr === CSR.mie) {
          MEIE := wdata(11)
          MTIE := wdata(7)
          MSIE := wdata(3)
        }
        .elsewhen(csr_addr === CSR.mtvec) { _mtvec.base := (wdata >> 2.U) }
        .elsewhen(csr_addr === CSR.mcycle) { cycle := wdata }
        .elsewhen(csr_addr === CSR.mcycleh) { cycleh := wdata }
        .elsewhen(csr_addr === CSR.minstret) { cycle := wdata }
        .elsewhen(csr_addr === CSR.minstreth) { cycleh := wdata }
        .elsewhen(csr_addr === CSR.mtime) { time := wdata }
        .elsewhen(csr_addr === CSR.mtimeh) { timeh := wdata }
        .elsewhen(csr_addr === CSR.mtimecmp) { mtimecmp := wdata }
        .elsewhen(csr_addr === CSR.mscratch) { mscratch := wdata }
        .elsewhen(csr_addr === CSR.mepc) { mepc := wdata >> 2.U << 2.U }
        .elsewhen(csr_addr === CSR.mcause) { mcause := wdata & (BigInt(1) << (xlen - 1) | 0xf).U }
        .elsewhen(csr_addr === CSR.mtval) { mtval := wdata }
        .elsewhen(csr_addr === CSR.mtohost) { mtohost := wdata }
        .elsewhen(csr_addr === CSR.mfromhost) { mfromhost := wdata }
        .elsewhen(csr_addr === CSR.cyclew) { cycle := wdata }
        .elsewhen(csr_addr === CSR.timew) { time := wdata }
        .elsewhen(csr_addr === CSR.instretw) { instret := wdata }
        .elsewhen(csr_addr === CSR.cyclehw) { cycleh := wdata }
        .elsewhen(csr_addr === CSR.timehw) { timeh := wdata }
        .elsewhen(csr_addr === CSR.instrethw) { instreth := wdata }
    }
  }
}
