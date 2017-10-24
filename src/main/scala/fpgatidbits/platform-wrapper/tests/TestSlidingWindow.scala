package fpgatidbits.Testbenches

import Chisel._

import fpgatidbits.dma._
import fpgatidbits.streams._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.rosetta._

class TestSlidingWindow(p: PlatformWrapperParams, _wordSize:Int) extends GenericAccelerator(p) {
  val wordSize = _wordSize 
  val io = new GenericAcceleratorIF(numMemPorts, p){
    val imageSizeX = UInt(INPUT, width=32)
    val imageSizeY = UInt(INPUT, width=32)
    val numChannels = UInt(INPUT, width=32)
    val stride = UInt(INPUT, width=32)
    val windowSize = UInt(INPUT, width=32) // Might want to tweakerino
    val addrImage = UInt(INPUT, width=64)
    val addrResult = UInt(INPUT, width=64)
  }
  
}
