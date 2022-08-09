// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import Instructions._
import junctions.NastiBundleParameters
import org.scalatest.flatspec.AnyFlatSpec

import scala.language.implicitConversions

trait DatapathTest
object BypassTest extends DatapathTest {
  override def toString: String = "bypass test"
}
object ExceptionTest extends DatapathTest {
  override def toString: String = "exception test"
}
// Define your own test
object BasicAdditionTest extends DatapathTest {
  override def toString(): String = "basic addition test"
}
object SimpleBranchJumpTest extends DatapathTest {
  override def toString(): String = "simple branch and jump test"
}

trait TestUtils {
  implicit def boolToBoolean(x:   Bool):   Boolean = x.litValue == 1
  implicit def bitPatToUInt(b:    BitPat): UInt = BitPat.bitPatToUInt(b)
  implicit def uintToBitPat(u:    UInt):   BitPat = BitPat(u)
  implicit def bigIntToInt(x:     BigInt): Int = x.toInt
  implicit def bigIntToBoolean(x: BigInt): Boolean = x != 0
  def toBigInt(x:                 Int):    BigInt = (BigInt(x >>> 1) << 1) | (x & 0x1)

  def rs1(inst: UInt) = ((inst.litValue >> 15) & 0x1f).toInt
  def rs2(inst: UInt) = ((inst.litValue >> 20) & 0x1f).toInt
  def rd(inst:  UInt) = ((inst.litValue >> 7) & 0x1f).toInt
  def csr(inst: UInt) = inst.litValue >> 20
  def reg(x:    Int) = (x & ((1 << 5) - 1)).U(5.W)
  def imm(x:    Int) = (x & ((1 << 20) - 1)).S(21.W)
  def Cat(l:    Seq[Bits]): UInt = l.tail.foldLeft(l.head.asUInt) { (x, y) =>
    assert(x.isLit && y.isLit)
    (x.litValue << y.getWidth | y.litValue).U((x.getWidth + y.getWidth).W)
  }
  def Cat(x: Bits, l: Bits*): UInt = Cat(x :: l.toList)
  val fence = Cat(0.U(4.W), 0xf.U(4.W), 0xf.U(4.W), 0.U(13.W), Opcode.MEMORY)
  val nop = Cat(0.U(12.W), reg(0), Funct3.ADD, reg(0), Opcode.ITYPE)
  val csrRegs = CSR.regs.map(_.litValue)
  private val csrMap = csrRegs
    .zip(
      List(
        "cycle",
        "time",
        "instret",
        "cycleh",
        "timeh",
        "instreth",
        "cyclew",
        "timew",
        "instretw",
        "cyclehw",
        "timehw",
        "instrethw",
        "mcpuid",
        "mimpid",
        "mhartid",
        "mtvec",
        "medeleg",
        "mideleg",
        "mie",
        "mtimecmp",
        "mtime",
        "mtimeh",
        "mscratch",
        "mepc",
        "mcause",
        "mbadaddr",
        "mip",
        "mtohost",
        "mfromhost",
        "mstatus"
      )
    )
    .toMap
  def csrName(csr: BigInt) = csrMap.getOrElse(csr, csr.toString(16))

  private def inst_31(inst:    UInt) = ((inst.litValue >> 31) & 0x1).U(1.W)
  private def inst_30_25(inst: UInt) = ((inst.litValue >> 25) & 0x3f).U(6.W)
  private def inst_24_21(inst: UInt) = ((inst.litValue >> 21) & 0xf).U(4.W)
  private def inst_20(inst:    UInt) = ((inst.litValue >> 20) & 0x1).U(1.W)
  private def inst_19_12(inst: UInt) = ((inst.litValue >> 12) & 0xff).U(8.W)
  private def inst_11_8(inst:  UInt) = ((inst.litValue >> 8) & 0xf).U(4.W)
  private def inst_7(inst:     UInt) = ((inst.litValue >> 7) & 0x1).U(1.W)

  def iimm(inst: UInt) = Cat(Cat(Seq.fill(21) { inst_31(inst) }), inst_30_25(inst), inst_24_21(inst), inst_20(inst))
  def simm(inst: UInt) = Cat(Cat(Seq.fill(21) { inst_31(inst) }), inst_30_25(inst), inst_11_8(inst), inst_7(inst))
  def bimm(inst: UInt) =
    Cat(Cat(Seq.fill(20) { inst_31(inst) }), inst_7(inst), inst_30_25(inst), inst_11_8(inst), 0.U(1.W))
  def uimm(inst: UInt) =
    Cat(inst_31(inst), inst_30_25(inst), inst_24_21(inst), inst_20(inst), inst_19_12(inst), 0.U(12.W))
  def jimm(inst: UInt) = Cat(
    Cat(Seq.fill(12) { inst_31(inst) }),
    inst_19_12(inst),
    inst_20(inst),
    inst_30_25(inst),
    inst_24_21(inst),
    0.U(1.W)
  )
  def zimm(inst: UInt) = ((inst.litValue >> 15) & 0x1f).U

  /* Define tests */
  val rnd = new scala.util.Random
  def rand_fn7 = rnd.nextInt(1 << 7).U(7.W)
  def rand_rs2 = (rnd.nextInt((1 << 5) - 1) + 1).U(5.W)
  def rand_rs1 = (rnd.nextInt((1 << 5) - 1) + 1).U(5.W)
  def rand_fn3 = rnd.nextInt(1 << 3).U(3.W)
  def rand_rd = (rnd.nextInt((1 << 5) - 1) + 1).U(5.W)
  def rand_csr = csrRegs(rnd.nextInt(csrRegs.size - 1)).U
  def rand_inst = toBigInt(rnd.nextInt()).U
  def rand_addr = toBigInt(rnd.nextInt()).U
  def rand_data = toBigInt(rnd.nextInt()).U

  val insts: Seq[UInt] = Seq(
    Cat(rand_fn7, rand_rs2, rand_rs1, rand_fn3, rand_rd, Opcode.LUI),
    Cat(rand_fn7, rand_rs2, rand_rs1, rand_fn3, rand_rd, Opcode.AUIPC),
    Cat(rand_fn7, rand_rs2, rand_rs1, rand_fn3, rand_rd, Opcode.JAL),
    Cat(rand_fn7, rand_rs2, rand_rs1, 0.U(3.W), rand_rd, Opcode.JALR),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BEQ, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BNE, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BLT, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BGE, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BLTU, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BGEU, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.LB, rand_rd, Opcode.LOAD),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.LH, rand_rd, Opcode.LOAD),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.LW, rand_rd, Opcode.LOAD),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.LBU, rand_rd, Opcode.LOAD),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.LHU, rand_rd, Opcode.LOAD),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.SB, rand_rd, Opcode.STORE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.SH, rand_rd, Opcode.STORE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.SW, rand_rd, Opcode.STORE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.ADD, rand_rd, Opcode.ITYPE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.SLT, rand_rd, Opcode.ITYPE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.SLTU, rand_rd, Opcode.ITYPE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.XOR, rand_rd, Opcode.ITYPE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.OR, rand_rd, Opcode.ITYPE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.AND, rand_rd, Opcode.ITYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SLL, rand_rd, Opcode.ITYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SR, rand_rd, Opcode.ITYPE),
    Cat(Funct7.S, rand_rs2, rand_rs1, Funct3.SR, rand_rd, Opcode.ITYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.ADD, rand_rd, Opcode.RTYPE),
    Cat(Funct7.S, rand_rs2, rand_rs1, Funct3.ADD, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SLL, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SLT, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SLTU, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.XOR, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SR, rand_rd, Opcode.RTYPE),
    Cat(Funct7.S, rand_rs2, rand_rs1, Funct3.SR, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.OR, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.AND, rand_rd, Opcode.RTYPE),
    fence,
    FENCEI,
    Cat(rand_csr, rand_rs1, Funct3.CSRRW, rand_rd, Opcode.SYSTEM),
    Cat(rand_csr, rand_rs1, Funct3.CSRRS, rand_rd, Opcode.SYSTEM),
    Cat(rand_csr, rand_rs1, Funct3.CSRRC, rand_rd, Opcode.SYSTEM),
    Cat(rand_csr, rand_rs1, Funct3.CSRRWI, rand_rd, Opcode.SYSTEM),
    Cat(rand_csr, rand_rs1, Funct3.CSRRSI, rand_rd, Opcode.SYSTEM),
    Cat(rand_csr, rand_rs1, Funct3.CSRRCI, rand_rd, Opcode.SYSTEM),
    ECALL,
    EBREAK,
    ERET,
    nop,
    rand_inst
  )

  def RU(funct3: UInt, rd: Int, rs1: Int, rs2: Int) =
    Cat(Funct7.U, reg(rs2), reg(rs1), funct3, reg(rd), Opcode.RTYPE)
  def RS(funct3: UInt, rd: Int, rs1: Int, rs2: Int) =
    Cat(Funct7.S, reg(rs2), reg(rs1), funct3, reg(rd), Opcode.RTYPE)
  def I(funct3: UInt, rd: Int, rs1: Int, i: Int) =
    Cat(imm(i)(11, 0), reg(rs1), funct3, reg(rd), Opcode.ITYPE)
  def L(funct3: UInt, rd: Int, rs1: Int, i: Int) =
    Cat(imm(i)(11, 0), reg(rs1), funct3, reg(rd), Opcode.LOAD)
  def S(funct3: UInt, rs2: Int, rs1: Int, i: Int) =
    Cat(imm(i)(11, 5), reg(rs2), reg(rs1), funct3, imm(i)(4, 0), Opcode.STORE)
  def B(funct3: UInt, rs1: Int, rs2: Int, i: Int) =
    Cat(imm(i)(12), imm(i)(10, 5), reg(rs2), reg(rs1), funct3, imm(i)(4, 1), imm(i)(11), Opcode.BRANCH)
  def U(op: UInt, rd: Int, i: Int) =
    Cat(imm(i), reg(rd), op)
  def J(rd: Int, i: Int) =
    Cat(imm(i)(20), imm(i)(10, 1), imm(i)(11), imm(i)(19, 12), reg(rd), Opcode.JAL)
  def JR(rd: Int, rs1: Int, i: Int) =
    Cat(imm(i)(11, 0), reg(rs1), 0.U(3.W), reg(rd), Opcode.JALR)
  def SYS(funct3: UInt, rd: Int, csr: UInt, rs1: Int) =
    Cat(csr, reg(rs1), funct3, reg(rd), Opcode.SYSTEM)

  val fin = Cat(CSR.mtohost, reg(31), Funct3.CSRRW, reg(0), Opcode.SYSTEM)
  val bypassTest = List(
    I(Funct3.ADD, 1, 0, 1), //0 ADDI x1, x0, 1   # x1 <- 1
    // I(Funct3.ADD, 1, 1, 1), // ADDI x1, x1, 1   # x1 <- 2
    // I(Funct3.ADD, 1, 1, 1), // ADDI x1, x1, 1   # x1 <- 3
    S(Funct3.SW, 1, 0, 12), //1 SW   x1, x0, 12  # Mem[12] <- 1
    L(Funct3.LW, 2, 0, 12), //2 LW   x2, x0, 12  # x2 <- 1
    RU(Funct3.ADD, 3, 2, 2), //3 ADD  x3, x2, x2  # x3 <- 2
    RS(Funct3.ADD, 4, 3, 2), //4 SUB  x4, x2, x3  # x4 <- 1
    RU(Funct3.SLL, 5, 3, 4), //5 SLL  x5, x2, x4  # x5 <- 4
    RU(Funct3.SLT, 6, 4, 5), //6 SLT  x6, x4, x5  # x6 <- 1
    B(Funct3.BEQ, 1, 6, 8), //7 BEQ  x1, x6, 8   # go to the BGE branch
    J(0, 12), //8 JAL  x0, 12      # skip nop
    B(Funct3.BGE, 4, 1, -4), //9 BGE  x4, x1, -4  # go to the jump
    nop, //10
    nop, //11
    RU(Funct3.ADD, 26, 0, 1), //12 ADD x26,  x0, x1  # x26 <- 1
    RU(Funct3.ADD, 27, 26, 2), //13 ADD x27, x26, x2  # x27 <- 2
    RU(Funct3.ADD, 28, 27, 3), //14 ADD x28, x27, x3  # x28 <- 4
    RU(Funct3.ADD, 29, 28, 4), //15 ADD x29, x28, x4  # x29 <- 5
    RU(Funct3.ADD, 30, 29, 5), //16 ADD x30, x29, x5  # x30 <- 9
    RU(Funct3.ADD, 31, 30, 6), //17 ADD x31, x31, x6  # x31 <- 10
    fin //18
  )

  val simpleBranchJumpTest = List(
    I(Funct3.ADD, 1, 0, 1), // ADDI x1, x0, 1   # x1 <- 1
    nop,
    nop,
    nop,
    // I(Funct3.ADD, 1, 1, 1), // ADDI x1, x1, 1   # x1 <- 2
    // I(Funct3.ADD, 1, 1, 1), // ADDI x1, x1, 1   # x1 <- 3
    S(Funct3.SW, 1, 0, 12), // SW   x1, x0, 12  # Mem[12] <- 1
    nop,
    nop,
    nop,
    L(Funct3.LW, 2, 0, 12), // LW   x2, x0, 12  # x2 <- Mem[12] (which is 1)
    nop,
    nop,
    nop,
    RU(Funct3.ADD, 3, 2, 2), // ADD  x3, x2, x2  # x3 <- 2
    nop,
    nop,
    nop,
    RS(Funct3.ADD, 4, 3, 2), // SUB  x4, x2, x3  # x4 <- 1
    nop,
    nop,
    nop,
    RU(Funct3.SLL, 5, 3, 4), // SLL  x5, x2, x4  # x5 <- 4
    nop,
    nop,
    nop,
    RU(Funct3.SLT, 6, 4, 5), // SLT  x6, x4, x5  # x6 <- 1
    nop,
    nop,
    nop,
    B(Funct3.BEQ, 1, 6, 8), // BEQ  x1, x6, 8   # go to the BGE branch
    J(0, 12), // JAL  x0, 12      # skip nop
    B(Funct3.BGE, 4, 1, -4), // BGE  x4, x1, -4  # go to the jump
    nop,
    nop,
    RU(Funct3.ADD, 26, 0, 1), // ADD x26,  x0, x1  # x26 <- 1
    nop,
    nop,
    nop,
    RU(Funct3.ADD, 27, 26, 2), // ADD x27, x26, x2  # x27 <- 2
    nop,
    nop,
    nop,
    RU(Funct3.ADD, 28, 27, 3), // ADD x28, x27, x3  # x28 <- 4
    nop,
    nop,
    nop,
    RU(Funct3.ADD, 29, 28, 4), // ADD x29, x28, x4  # x29 <- 5
    nop,
    nop,
    nop,
    RU(Funct3.ADD, 30, 29, 5), // ADD x30, x29, x5  # x30 <- 9
    nop,
    nop,
    nop,
    RU(Funct3.ADD, 31, 30, 6), // ADD x31, x31, x6  # x31 <- 10
    nop,
    nop,
    nop,
    fin
  )

  val exceptionTest = List(
    fence,
    I(Funct3.ADD, 31, 0, 2), // ADDI x31, x0,  1 # x31 <- 2
    I(Funct3.ADD, 31, 31, 1), // ADDI x31, x31, 1 # x31 <- 3
    I(Funct3.ADD, 31, 31, 1), // ADDI x31, x32, 1 # x31 <- 4
    0.U, // exception
    I(Funct3.ADD, 31, 31, 1), // ADDI x31, x31, 1 # x31 <- 5
    I(Funct3.ADD, 31, 31, 1), // ADDI x31, x31, 1 # x31 <- 6
    I(Funct3.ADD, 31, 31, 1), // ADDI x31, x31, 1 # x31 <- 7
    fin
  )
  val basicAdditionTest = List(
    I(Funct3.ADD, 1, 0, 1), // ADDI x1, x0, 1   # x1 <- 1
    // nop,
    // nop,
    // nop,
    I(Funct3.ADD, 1, 1, 1), // ADDI x1, x1, 1   # x1 <- 2
    // nop,
    // nop,
    // nop,
    I(Funct3.ADD, 1, 1, 1), // ADDI x1, x1, 1   # x1 <- 3
    I(Funct3.ADD, 1, 1, 1), // ADDI x1, x1, 1   # x1 <- 4
    // nop,
    // nop,
    // nop,
    I(Funct3.ADD, 2, 1, 1), // ADDI x2, x1,  1 # x2 <- 5
    I(Funct3.ADD, 3, 2, 1), // ADDI x3, x2,  1 # x3 <- 6
    I(Funct3.ADD, 31, 0, 0), // ADDI x31, x0,  0 # x31 <- 0
    // nop,
    // nop,
    // nop,
    RU(Funct3.ADD, 31, 3, 2), // ADD x31, x3, x2  # x31 <- 11
    // nop,
    // nop,
    // nop,
    fin
  )

  val tests =
    Map(
      BypassTest -> bypassTest,
      SimpleBranchJumpTest -> simpleBranchJumpTest,
      ExceptionTest -> exceptionTest,
      BasicAdditionTest -> basicAdditionTest
    )
  val testResults = Map(
    SimpleBranchJumpTest -> 10,
    BypassTest -> 10,
    ExceptionTest -> 4,
    BasicAdditionTest -> 11
  )
}

object TestUtils {
  def resizeHexFile(originalHexFile: os.Path, resizedHexFile: os.Path, resizedWidth: Int): Unit = {
    val hexFileLines = os.read.lines(originalHexFile)
    val resizedLines = resizeHexFileInner(hexFileLines, resizedWidth)
    os.write.over(resizedHexFile, resizedLines.mkString("\n"), createFolders = true)
  }

  def resizeHexFileInner(hexFileLines: Seq[String], resizedWidth: Int): Seq[String] = {
    val hexFileLineLengths = hexFileLines.filter(_.nonEmpty).map(_.length)
    assert(hexFileLineLengths.forall(_ == hexFileLineLengths.head), "hex file has lines of differing lengths")
    val originalWidth = hexFileLineLengths.head * 4 // 4 bits / nibble

    if (originalWidth > resizedWidth) {
      assert(
        originalWidth % resizedWidth == 0,
        f"The original width of the hex file ($originalWidth) is not evenly divisible by the desired resized width ($resizedWidth)"
      )

      hexFileLines.flatMap(_.grouped(resizedWidth / 4).toSeq.reverse)
    } else if (originalWidth < resizedWidth) {
      ???
    } else { // no resizing
      hexFileLines
    }
  }
}

class TestUtilsTests extends AnyFlatSpec {
  "resizeHexFile" should "resize to a smaller word size" in {
    val in = Seq("12345678deadbeef1010101066666666", "11112222333344445555666677778888")
    val out = TestUtils.resizeHexFileInner(in, 32)
    assert(out == Seq("66666666", "10101010", "deadbeef", "12345678", "77778888", "55556666", "33334444", "11112222"))

    val out2 = TestUtils.resizeHexFileInner(in, 64)
    assert(out2 == Seq("1010101066666666", "12345678deadbeef", "5555666677778888", "1111222233334444"))
  }
}

object TestParams {
  val Nasti = NastiBundleParameters(addrBits = 32, dataBits = 64, idBits = 5)
}
