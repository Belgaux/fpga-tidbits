package fpgatidbits.Testbenches

import Chisel._

import fpgatidbits.dma._
import fpgatidbits.ocm._
import fpgatidbits.streams._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.rosetta._


// Ties together the sliding window module and the matrix multiplication module
class TestConvolution(p: PlatformWrapperParams, _wordSizeInBits:Int) extends GenericAccelerator(p) {
  val wordSizeInBits = _wordSizeInBits
  val wordSizeInBytes = wordSizeInBits/8
  val numMemPorts = 2

  val io = new GenericAcceleratorIF(numMemPorts, p){

  }

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

  val reader = initialize_reader(0) 
  val writer = initialize_writer(1)

  val readerPort = 0
  val writerPort = 0

  val windowSlider = Module(new ModuleSlidingWindowBitplanes(p, wordSizeInBits)).io

  windowSlider.readerIF <> reader
  windowSlider.writerIF <> writer
}
 
