package mini.synth

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.options.TargetDirAnnotation

import mini.{CoreConfig, MiniConfig}
import mini.DatapathStages.{FetchStage, FetchStageIO}

// synthesizeable modules from rv-mini stages -- buffered IO

class CoreSynth(val conf: CoreConfig) extends Module {

  val fetch = Module(new FetchSynth(conf))

}

class FetchSynth(val conf: CoreConfig) extends Module {
  val fetchSynth = Module(new FetchStage(conf))

  fetchSynth.io := Reg(new FetchStageIO(conf.xlen))

}

object Main extends App {
  val targetDirectory = args.head
  val config = MiniConfig()
  new chisel3.stage.ChiselStage().execute(
    args,
    Seq(
      ChiselGeneratorAnnotation(() => new CoreSynth(config.core)),
      TargetDirAnnotation(targetDirectory)
    )
  )
}
