package mini.synth

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.options.TargetDirAnnotation

import mini.{CoreConfig, MiniConfig}
import mini.DatapathStages.{FetchStage, FetchStageIO}

// synthesizeable modules from rv-mini stages -- buffered IO
// Broadcast global signals need to be registered
// Fetch Stage -- icache IO need to be registered
// Decode Stage -- register global signals (full_stall, dec_stall etc.) and check bypass paths
// Execute Stage -- same as decode stage
// Memory Stage -- CSR outputs need to be registered and check dcache IO
// Writeback Stage -- ?? nothing to do

class CoreSynth(val conf: CoreConfig) extends Module {
  val fetch = Module(new FetchSynth(conf))
  val decode = Module(new DecodeSynth(conf))
  val execute = Module(new ExecuteSynth(conf))
  val memory = Module(new MemorySynth(conf))
  val writeback = Module(new WtirebackSynth(conf))
}
class FetchSynth(val conf: CoreConfig) extends Module {
  val fetchSynth = Module(new FetchStage(conf))
  fetchSynth.io := Reg(new FetchStageIO(conf.xlen))
}
class DecodeSynth(val conf: CoreConfig) extends Module {
  val decodeSynth = Module(new DecodeStage(conf))
  decodeSynth.io := Reg(new DecodeStageIO(conf.xlen))
}
class ExecuteSynth(val conf: CoreConfig) extends Module {
  val executeSynth = Module(new ExecuteStage(conf))
  executeSynth.io := Reg(new ExecuteStageIO(conf.xlen))
}

class MemorySynth(val conf: CoreConfig) extends Module {
  val memorySynth = Module(new MemoryStage(conf))
  memorySynth.io := Reg(new MemoryStageIO(conf.xlen))
}
class WritebackSynth(val conf: CoreConfig) extends Module {
  val writebackSynth = Module(new WritebackStage(conf))
  writebackSynth.io := Reg(new WritebackStageIO(conf.xlen))
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
