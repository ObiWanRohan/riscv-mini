package adder

import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.options.TargetDirAnnotation

object Main extends App {
  val targetDirectory = args.head

  val width = 16

  new chisel3.stage.ChiselStage().execute(
    args,
    Seq(
      ChiselGeneratorAnnotation(() => new CarryRippleAdder(width)),
      TargetDirAnnotation(targetDirectory)
    )
  )
}
