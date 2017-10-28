package fpgatidbits.Testbenches

import Chisel._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.rosetta._

class TestBitserial(p: PlatformWrapperParams) extends GenericAccelerator(p) {
  val ws = 32
  val numMemPorts = 0
  val io = new GenericAcceleratorIF(numMemPorts, p) {
    val start = Bool(INPUT)
    val bitplane = Bool(INPUT)
    val W = SInt(INPUT, width = ws)
    val A = SInt(INPUT, width = ws)
    val done = Bool(OUTPUT)
    val out = SInt(OUTPUT, width = ws)
  }

  val PE = Module(new Bitserial(ws, 2, 2)).io
  PE.start := io.start
  PE.bitplane := io.bitplane
  PE.W := io.W
  PE.A := io.A
  io.done := PE.done
  io.out := PE.out

}
