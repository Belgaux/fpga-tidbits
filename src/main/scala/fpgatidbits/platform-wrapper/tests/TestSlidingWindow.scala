package fpgatidbits.Testbenches

import Chisel._

import fpgatidbits.dma._
import fpgatidbits.streams._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.rosetta._

class TestSlidingWindow(p: PlatformWrapperParams, _wordSize:Int) extends GenericAccelerator(p) {
  val wordSize = _wordSize 
  val numMemPorts = 2

  val io = new GenericAcceleratorIF(numMemPorts, p){
    // Inputs
    val imageSizeX =  UInt(INPUT, width=32)
    val imageSizeY =  UInt(INPUT, width=32)
    val numChannels = UInt(INPUT, width=32)
    val stride =      UInt(INPUT, width=32)
    val windowSize =  UInt(INPUT, width=32) // Might want to tweakerino
    //val outChannels = UInt(INPUT, width=32)
    val addrImage =   UInt(INPUT, width=64)
    val addrResult =  UInt(INPUT, width=64)
    //val addrWeights = UInt(INPUT, width=64)
    val start =       Bool(INPUT)

    // Outputs
    val finished = Bool(OUTPUT)
  }

  /** Returns an initialized reader
    * @note memwriteport is plugged
    * @note reader is default false
    */  
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

  def initialize_writer (port: UInt) = {
    val wr = Module(new StreamWriter(new StreamWriterParams(
      streamWidth = p.memDataBits, mem = p.toMemReqParams(), chanID = 0
    ))).io
    wr.start := Bool(false)
    wr.req <> io.memPort(port).memWrReq
    wr.wdat <> io.memPort(port).memWrDat
    io.memPort(port).memWrRsp <> wr.rsp
    plugMemReadPort(port)
    wr
  }

  // Set initial state
  val s_idle :: s_read :: s_lowering :: Nil = Enum(UInt(), 3) 
  val state = Reg(init=UInt(s_idle))
  
  // Reader is default false
  val reader = initialize_reader(0) 
  val writer = initialize_writer(1)
  switch (state){
    is(s_idle){
      when (io.start){
        state := s_read
      }
    }
    is(s_read){
      reader.baseAddr := AddrWeights
      reader.byteCount := windowsize * numChannels
      reader.start := Bool(true)
      reader.out <> writer.in 
    }
    is(s_lowering){
     
    }
  }
}
