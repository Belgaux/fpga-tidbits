package fpgatidbits.Testbenches

import Chisel._

import fpgatidbits.dma._
import fpgatidbits.ocm._
import fpgatidbits.streams._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.rosetta._


// Expects input of form channel/bits/rows/columns, with every row padded to wordsize bits
class TestSlidingWindowBitplanes(p: PlatformWrapperParams, _wordSizeInBits:Int) extends GenericAccelerator(p) {
  val wordSizeInBits = _wordSizeInBits
  val wordSizeInBytes = wordSizeInBits/8
  val numMemPorts = 2

  val io = new GenericAcceleratorIF(numMemPorts, p){
    val numCols = UInt(INPUT, width=16)
    val numRows = UInt(INPUT, width=16)
    val numBits = UInt(INPUT, width=16)
    val numChannels = UInt(INPUT, width=16)
    val windowSize = UInt(INPUT, width=16)
    val stride = UInt(INPUT, width=16)
    val addrImage =  UInt(INPUT, width=wordSizeInBits)
    val addrResult = UInt(INPUT, width=wordSizeInBits)
    val start = Bool(INPUT)

    val finished = Bool(OUTPUT)

    // Debug:
    val checkAddrBRAM = UInt(INPUT, width=32)
    val debugOutput = UInt(OUTPUT, width=wordSizeInBits)
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
  val s_idle :: s_slide :: s_fill_bram :: s_write_buffer :: s_finished :: Nil = Enum(UInt(), 5) 
  val state = Reg(init=UInt(s_idle))

  val reader = initialize_reader(0) 
  val writer = initialize_writer(1)
  reader.out <> writer.in

  reader.baseAddr := UInt(0)
  reader.byteCount := UInt(0)
  reader.out.ready := Bool(false)

  val bram = Module(new DualPortBRAM(
    addrBits = 14, // 14 bits is enough for 10000 columns and a windowSize of 16
    dataBits = wordSizeInBits
  )).io

  val bramWritePort = bram.ports(0)
  val bramReadPort = bram.ports(1)

  bramWritePort.req.addr := UInt(0)
  bramWritePort.req.writeData := UInt(0)
  bramWritePort.req.writeEn := Bool(false)

  bramReadPort.req.writeEn := Bool(false)
  bramReadPort.req.addr := UInt(0)
  io.finished := Bool(false)

  // Debug
  bramReadPort.req.addr := io.checkAddrBRAM
  io.debugOutput := bramReadPort.rsp.readData

  val buffer = Reg(init=UInt(width=wordSizeInBits))

  val currInputRow = UInt(width=16)
  val currInputCol = UInt(width=16)
  val currInputBitplane = UInt(width=16)
  val currInputChannel = UInt(width=16)

  val currOutputRow = Reg(init=UInt(0, width=32))
  val currOutputBitplane = Reg(init=UInt(0, width=32))

  val currInputFillingRow = Reg(init=UInt(0, width=16))
  val currOutputFillingRow = Reg(init=UInt(0, width=16))
  val currStartBRAMRow = Reg(init=UInt(0, width=16))

  val wordBitExponent = log2Up(wordSizeInBits)
  val wordByteExponent = wordBitExponent - 3
  val inputBytesPerRow = (((io.numCols + UInt(wordSizeInBits - 1)) >> wordBitExponent) << wordByteExponent)
  val inputBitplaneSize = Reg(init=UInt(0, width=16))
  val inputChannelSize = Reg(init=UInt(0, width=32))
  val inputBitsChannelsOffset = Reg(init=UInt(0, width=32))

  val justFilledBRAM = Reg(init=Bool(false))

  inputBitplaneSize := io.numRows * inputBytesPerRow
  inputChannelSize := io.numBits * inputBitplaneSize
  inputBitsChannelsOffset := inputChannelSize * currInputChannel + inputBitplaneSize * currInputBitplane

  switch (state){
    is(s_idle){
      when(io.start){
        state := s_fill_bram
      }
    }

    is(s_slide){
      when(((currInputCol & (UInt(wordSizeInBits - 1))) === UInt(0)) && !justFilledBRAM){
        state := s_fill_bram
        currOutputFillingRow := currStartBRAMRow
        currInputFillingRow := UInt(0)
      }.otherwise{
        // Do some filling of buffer from BRAM
        state := s_finished
      }
    }

    is(s_fill_bram){
      reader.baseAddr := io.addrImage + inputBitsChannelsOffset + (currInputRow + currInputFillingRow) * inputBytesPerRow + (currInputCol >> (wordBitExponent - wordByteExponent))
      reader.byteCount := UInt(wordSizeInBytes)
      reader.start := Bool(true)
      reader.out.ready := Bool(true)
      bramWritePort.req.addr := currOutputFillingRow * (inputBytesPerRow >> wordByteExponent) + (currInputCol >> wordBitExponent)
      bramWritePort.req.writeEn := reader.out.valid
      bramWritePort.req.writeData := reader.out.bits

      when(reader.out.valid){

        printf("Writing %b to address %d\n", bramWritePort.req.writeData, bramWritePort.req.addr)
        reader.start := Bool(false)
        when(currInputFillingRow === io.windowSize - UInt(1)){
          justFilledBRAM := Bool(true)
          state := s_slide
        }.otherwise{
          currInputFillingRow := currInputFillingRow + UInt(1)
          when(currOutputFillingRow === io.windowSize - UInt(1)){
            currOutputFillingRow := UInt(0)
          }.otherwise{
            currOutputFillingRow := currOutputFillingRow + UInt(1)
          }
        }
      }
    }


    is(s_finished){
      io.finished := Bool(true)
      when(~io.start){
        state := s_idle
      }
    }
  }
}
