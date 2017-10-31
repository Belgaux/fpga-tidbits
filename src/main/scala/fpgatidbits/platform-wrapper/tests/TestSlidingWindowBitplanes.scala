package fpgatidbits.Testbenches

import Chisel._

import fpgatidbits.dma._
import fpgatidbits.streams._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.rosetta._

class TestSlidingWindowBitplanes(p: PlatformWrapperParams, _wordSizeInBits:Int) extends GenericAccelerator(p) {
  val wordSizeInBits = _wordSizeInBits
  val wordSizeInBytes = wordSizeInBits/8
  val numMemPorts = 2

  val io = new GenericAcceleratorIF(numMemPorts, p){

  }

  /** Returns an initialized reader
    * @note memwriteport is plugged
    * @note reader is default false
    */  
  def initialize_reader(port: Int) = {
    val reader = Module(new StreamReader(new StreamReaderParams(
      streamWidth = wordSizeInBits, fifoElems = 8, mem = p.toMemReqParams(),
      maxBeats = 1, chanID = 0, disableThrottle = true
    ))).io
    reader.start := Bool(false) 
    plugMemWritePort(port)
    reader.req <> io.memPort(port).memRdReq
    reader.rsp <> io.memPort(port).memRdRsp
    reader
  }

  def initialize_writer (port: Int) = {
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
  val s_idle :: s_read :: s_finished :: Nil = Enum(UInt(), 3) 
  val state = Reg(init=UInt(s_idle))

  val reader = initialize_reader(0) 
  val writer = initialize_writer(1)
  reader.out <> writer.in

  switch (state){
    is(s_idle){
    }

    is(s_read){
    }

    is(s_finished){
    }
  }
}
