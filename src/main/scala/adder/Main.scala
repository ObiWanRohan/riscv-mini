package adder

import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.options.TargetDirAnnotation
import firrtl.stage.OutputFileAnnotation

object Main extends App {
  val targetDirectory = args.head

  val width = 32

  new chisel3.stage.ChiselStage().execute(
    args,
    Seq(
      ChiselGeneratorAnnotation(() => new MultipleAdders(width)),
      TargetDirAnnotation(targetDirectory)
    )
  )
}
