// See LICENSE for license details.

package mini

import junctions.NastiBundleParameters

case class Config(core: CoreConfig, cache: CacheConfig, nasti: NastiBundleParameters)

object MiniConfig {
  def apply(): Config = {
    val xlen = 32
    val numWays = 1
    Config(
      core = CoreConfig(
        trace = false,
        traceStack = false,
        numWays = 32,
        xlen = xlen,
        makeAlu = new AluSimple(_),
        makeBrCond = new BrCondSimple(_),
        makeImmGen = new ImmGenWire(_),
        makeForwardingUnit = new ForwardingUnit(_, _)
      ),
      cache = CacheConfig(
        nWays = 1,
        nSets = 256,
        blockBytes = 4 * (xlen / 8) // 4 * 32 bits = 16B
      ),
      nasti = NastiBundleParameters(
        addrBits = 32,
        dataBits = 64,
        idBits = 5
      )
    )
  }
}
