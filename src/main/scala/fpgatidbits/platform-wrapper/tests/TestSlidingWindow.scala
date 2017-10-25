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
    val numCols =     UInt(INPUT, width=32)
    val numRows =     UInt(INPUT, width=32)
    val numChannels = UInt(INPUT, width=32)
    val stride =      UInt(INPUT, width=32)
    val windowSize =  UInt(INPUT, width=32) // Might want to tweakerino
    //val outChannels = UInt(INPUT, width=32)
    val addrImage =   UInt(INPUT, width=64)
    val addrResult =  UInt(INPUT, width=64)
    val addr = UInt(INPUT, width=64)
    val start =       Bool(INPUT)
    // Outputs
    val finished = Bool(OUTPUT)
  }

  /** Returns an initialized reader
    * @note memwriteport is plugged
    * @note reader is default false
    */  
  def initialize_reader(port: Int) = {
    val reader = Module(new StreamReader(new StreamReaderParams(
      streamWidth = wordSize, fifoElems = 8, mem = p.toMemReqParams(),
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
  
  io.finished := Bool(false)

  // Set initial state
  val s_idle :: s_read :: s_finished :: Nil = Enum(UInt(), 3) 
  val state = Reg(init=UInt(s_idle))
  
  // Reader, and writer is default false
  val reader = initialize_reader(0) 
  val writer = initialize_writer(1)
  reader.out <> writer.in
  
  // For checking how many rows we have read
  val localRowCount = Reg(init=UInt(width=32))
  val globalRowCount = Reg(init=UInt(width=32))
  
  // Indexing in result
  val resultColCount = Reg(init=UInt(width=32))

  // Avoid negative slack by storing this
  val windowSizeSquared = Reg(init=UInt(width=32))
  windowSizeSquared := io.windowSize * io.windowSize
  
  val globalColCount = Reg(init=UInt(width=32))

  switch (state){
    is(s_idle){
      localRowCount := UInt(0)
      globalRowCount := UInt(0)
      resultColCount := UInt(0) 
      globalColCount := UInt(0)
      when (io.start){
        state := s_read
      }
    }
    is(s_read){
      reader.baseAddr := (io.addrImage + 
                          (globalRowCount + localRowCount) * io.numCols * io.numChannels +
                          globalColCount)
      reader.byteCount := io.windowSize * io.numChannels
      writer.baseAddr := io.addrResult + (resultColCount * windowSizeSquared) 
      writer.start := Bool(true)
      reader.start := Bool(true)
      when (reader.finished){
        when (localRowCount === io.windowSize - UInt(1)){
          resultColCount := resultColCount + UInt(1)
          writer.start := Bool(false)
          when (globalColCount === io.numCols - io.windowSize - UInt(1)){
            globalRowCount := globalRowCount + UInt(1)
            localRowCount := globalRowCount + UInt(1)
            globalColCount := UInt(0)
            when (globalRowCount === io.numRows - io.windowSize -UInt(1)){
              state := s_finished
            }
          }.otherwise{
            globalColCount := globalColCount + UInt(1)
          }
        }.otherwise{
          localRowCount := localRowCount + UInt(1)
        }
        reader.start := Bool(false) 
      }
    }
    is(s_finished){
      io.finished := Bool(true) 
      when (io.start === Bool(false)){
        state := s_idle
      }
    }
  }
}
