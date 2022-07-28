import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

package CPUControlSignalTypes {

  object PCSel extends ChiselEnum {
    val PC_4, PC_ALU, PC_0, PC_EPC = Value
  }

  object ASel extends ChiselEnum {
    val A_XXX, A_PC, A_RS1 = Value
  }

  object BSel extends ChiselEnum {
    val B_XXX, B_IMM, B_RS2 = Value
  }

  object ImmSel extends ChiselEnum {
    val IMM_X, IMM_I, IMM_S, IMM_U, IMM_J, IMM_B, IMM_Z = Value
  }

  object BrType extends ChiselEnum {
    val BR_XXX, BR_LTU, BR_LT, BR_EQ, BR_GEU, BR_GE, BR_NE = Value
  }

  object StType extends ChiselEnum {
    val ST_XXX, ST_SW, ST_SH, ST_SB = Value
  }

  object LdType extends ChiselEnum {
    val LD_XXX, LD_LW, LD_LH, LD_LB, LD_LHU, LD_LBU = Value
  }

  object WbSel extends ChiselEnum {
    val WB_ALU, WB_MEM, WB_PC4, WB_CSR = Value
  }

}
