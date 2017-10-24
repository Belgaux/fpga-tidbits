package fpgatidbits.Testbenches

import Chisel._

import fpgatidbits.dma._
import fpgatidbits.streams._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.rosetta._

class TestSlidingWindow(p: PlatformWrapperParams, _wordSize:Int) extends GenericAccelerator(p) {
  val wordSize = _wordSize 
  val numMemPorts = 1

  val io = new GenericAcceleratorIF(numMemPorts, p){
    // Inputs
    val imageSizeX =  UInt(INPUT, width=32)
    val imageSizeY =  UInt(INPUT, width=32)
    val numChannels = UInt(INPUT, width=32)
    val stride =      UInt(INPUT, width=32)
    val windowSize =  UInt(INPUT, width=32) // Might want to tweakerino
    val addrImage =   UInt(INPUT, width=64)
    val addrResult =  UInt(INPUT, width=64)
    val start =       Bool(INPUT)

    // Outputs
    val finished = Bool(OUTPUT)
  }
  
  def initialize_reader(port: UInt) = {
    val reader = Module(new StreamReader(new StreamReaderParams(
      streamWidth = wordSize, fifoElems = 8, mem = p.toMemReqParams(),
      maxBeats = 1, chanID = 0, disableThrottle = true
    ))).io
    reader.out.ready := Bool(false)
    reader.start := Bool(false) 
    plugMemWritePort(port)
    reader.req <> io.memPort(port).memRdReq 
    reader.rsp <> io.memPort(port).memRdRsp
    reader
  }
  
  // Set initial state
  val s_idle :: s_read :: s_lowering :: Nil = Enum(UInt(), 3) 
  val state = Reg(init=UInt(s_idle))
  
  // Reader is default false
  reader = initialize_reader(0) 

  switch (state){
    is(s_idle){
      when (io.start){
        state := s_read
      }
    }
    is(s_read){
      reader.start :=
      reader.baseAddr :=
      reader.byteCount :=

    }
    is(s_lowering){}
  }
}
