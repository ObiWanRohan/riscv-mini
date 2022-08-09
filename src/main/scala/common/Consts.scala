package mini.common

import chisel3._

object RISCVConstants {

  // abstract out instruction decode magic numbers

  val REG_ADDR_WIDTH = 5.W

  // Destination register address
  val RD_MSB = 11
  val RD_LSB = 7

  // Source register addresses
  val RS1_MSB = 19
  val RS1_LSB = 15
  val RS2_MSB = 24
  val RS2_LSB = 20

  // CSR Register Address
  val CSR_ADDR_MSB = 31
  val CSR_ADDR_LSB = 20

  val X0 = 0.U

  // The Bubble Instruction (Machine generated NOP)
  // Insert (XOR x0,x0,x0) which is different from software compiler
  // generated NOPs which are (ADDI x0, x0, 0).
  // Reasoning for this is to let visualizers and stat-trackers differentiate
  // between software NOPs and machine-generated Bubbles in the pipeline.
  val BUBBLE = 0x4033.U(32.W)

}
