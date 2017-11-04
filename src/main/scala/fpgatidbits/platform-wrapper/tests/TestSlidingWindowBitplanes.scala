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
  val s_idle :: s_fill_bram :: s_fill_window_size_buffer :: s_transfer_window_size_buffer :: s_write_buffer :: s_finished :: Nil = Enum(UInt(), 6) 
  val state = Reg(init=UInt(s_idle))

  val reader = initialize_reader(0) 
  val writer = initialize_writer(1)


  val writerOnLastCycle = Reg(init=Bool(false))
  when(writer.active){
    writerOnLastCycle := Bool(true)
    writer.start := Bool(true)
  }.otherwise{
    writerOnLastCycle := Bool(false)
    writer.start := Bool(false)
  }

  reader.baseAddr := UInt(0)
  reader.byteCount := UInt(0)
  reader.out.ready := Bool(false)

  writer.start := Bool(false)
  writer.baseAddr := UInt(0)
  writer.byteCount := UInt(0)
  writer.in.valid := Bool(false)
  writer.in.bits := UInt(0)

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

  val currInputRow = Reg(init=UInt(width=16))
  val currInputCol = Reg(init=UInt(width=32))
  val currInputBitplane = Reg(init=UInt(width=16))
  val currInputChannel = Reg(init=UInt(width=16))

  val currOutputRow = Reg(init=UInt(0, width=32))
  val currOutputBitplane = Reg(init=UInt(0, width=32))
  val currOutputWord = Reg(init=UInt(0, width=32))
  val currOutputBitInWord = Reg(init=UInt(0, width=32))

  val bramInputFillingRow = Reg(init=UInt(0, width=16))
  val bramInputFillingColWord = Reg(init=UInt(0, width=16))
  val bramOutputFillingRow = Reg(init=UInt(0, width=16))
  val currStartBRAMRow = Reg(init=UInt(0, width=16))
  val numBRAMRowsToFill = Reg(init=UInt(0, width=16))

  val wBufferFillReadColumnWord = Reg(init=UInt(0, width=32))
  val wBufferFillReadColumnBitInWord = Reg(init=UInt(0, width=32))
  val wBufferFillReadRow = Reg(init=UInt(0, width=16))
  val wBufferFillNumRowsRead = Reg(init=UInt(0, width=16))
  val wBufferFillWritePosition = Reg(init=UInt(0, width=16))
  val wBufferFillValidReadBRAM = Reg(init=Bool(false))
  val wBufferFillNumBitsReadOnRow = Reg(init=UInt(0, width=16))

  val temporaryBuffer = Reg(init=UInt(width=256)) // Used in moving from BRAM to writebuffer - 256 bits is enough for a 16 x 16 window
  val bufferToWrite = Reg(init=UInt(width=wordSizeInBits))

  val bufferTransferWriteCurrCol = Reg(init=UInt(width=wordSizeInBits))
  val bufferTransferReadCurrCol = Reg(init=UInt(width=wordSizeInBits))

  wBufferFillValidReadBRAM := Bool(false) // Should only be true if explicitly told to. Best to be safe

  val currInputColWord = currInputCol >> wordBitExponent
  val currInputColBitInWord = currInputCol & UInt(wordSizeInBits - 1)

  // Convenient constants
  val wordBitExponent = log2Up(wordSizeInBits)
  val wordByteExponent = wordBitExponent - 3
  val inputWordsPerRow = ((io.numCols + UInt(wordSizeInBits - 1)) >> wordBitExponent)
  val inputBytesPerRow = (inputWordsPerRow << wordByteExponent)

  val inputBitplaneSizeInBytes = Reg(init=UInt(0, width=16))
  val inputChannelSizeInBytes = Reg(init=UInt(0, width=32))
  val inputBitsChannelsOffset = Reg(init=UInt(0, width=32))

  val windowSizeSquared = Reg(init=UInt(0, width=16))
  val outputRowSizeInWords  = (windowSizeSquared * io.numChannels + UInt(wordSizeInBits - 1)) >> wordBitExponent
  val outputBitplaneSizeInBytes = Reg(init=UInt(0, width=32))
  val outputNumRowsPerBitplane = Reg(init = UInt(0, width = 32))

  val windowSizeMask = Reg(init = UInt(0, width=16))

  inputBitplaneSizeInBytes := io.numRows * inputBytesPerRow
  inputChannelSizeInBytes := io.numBits * inputBitplaneSizeInBytes
  inputBitsChannelsOffset := inputChannelSizeInBytes * currInputChannel + inputBitplaneSizeInBytes * currInputBitplane

  windowSizeSquared := io.windowSize * io.windowSize
  outputNumRowsPerBitplane := (io.numRows - io.windowSize + UInt(1)) * (io.numCols - io.windowSize + UInt(1))
  outputBitplaneSizeInBytes := (outputNumRowsPerBitplane * outputRowSizeInWords) << wordByteExponent

  windowSizeMask := (UInt(1) << io.windowSize) - UInt(1)

  switch (state){
    is(s_idle){
      when(io.start){
        state := s_fill_bram
        numBRAMRowsToFill := io.windowSize
        bramOutputFillingRow := UInt(0)
        bramInputFillingColWord := UInt(0)
        bramInputFillingRow := UInt(0)
        currInputCol := UInt(0)
        currInputRow := UInt(0)
        currInputBitplane := UInt(0)
        currInputChannel := UInt(0)
        currStartBRAMRow := UInt(0)

        currOutputBitplane := UInt(0)
        currOutputRow := UInt(0)
      }
    }

    is(s_fill_bram){
      reader.baseAddr := io.addrImage + inputBitsChannelsOffset + (currInputRow + bramInputFillingRow) * inputBytesPerRow
      reader.byteCount := inputWordsPerRow << wordByteExponent
      reader.start := Bool(true)
      reader.out.ready := Bool(true)
      bramWritePort.req.addr := bramOutputFillingRow * inputWordsPerRow + bramInputFillingColWord
      bramWritePort.req.writeEn := reader.out.valid
      bramWritePort.req.writeData := reader.out.bits

      when(reader.out.valid){ // Another word is transferred
        printf("Writing %b to address %d\n", bramWritePort.req.writeData, bramWritePort.req.addr)
        reader.start := Bool(false)
        when(bramInputFillingColWord === inputWordsPerRow - UInt(1)){ // Finished reading in row
          bramInputFillingColWord := UInt(0)
          when(bramInputFillingRow === numBRAMRowsToFill - UInt(1)){ // Finished all rows that should be read
            when(bramOutputFillingRow === io.windowSize - UInt(1)){ // Must start to fill BRAM from top next time
              currStartBRAMRow := UInt(0)
              wBufferFillReadRow := UInt(0)
            }.otherwise{
              currStartBRAMRow := currStartBRAMRow + UInt(1)
              wBufferFillReadRow := currStartBRAMRow + UInt(1)
            }
            wBufferFillWritePosition := UInt(0)
            wBufferFillReadColumnWord := currInputColWord
            wBufferFillReadColumnBitInWord := currInputColBitInWord
            wBufferFillValidReadBRAM := Bool(false)
            temporaryBuffer := UInt(0)
            wBufferFillNumBitsReadOnRow := UInt(0)
            wBufferFillNumRowsRead := UInt(0)

            bufferTransferWriteCurrCol := UInt(0)
            bufferTransferReadCurrCol := UInt(0)
            bufferToWrite := UInt(0)

            state := s_fill_window_size_buffer
          }.otherwise{
            bramInputFillingRow := bramInputFillingRow + UInt(1)
            when(bramOutputFillingRow === io.windowSize - UInt(1)){
              bramOutputFillingRow := UInt(0)
            }.otherwise{
              bramOutputFillingRow := bramOutputFillingRow + UInt(1)
            }
          }
        }.otherwise{
          bramInputFillingColWord := bramInputFillingColWord + UInt(1)
        }
      }
    }

    is(s_fill_window_size_buffer){
      printf("Tick in fill winBuff\n")
      val remainMask = Reg(init=UInt(0, width=16))
      val lastCycleColBitInWord = Reg(init=UInt(0, width=16))
      val lastStride = Reg(init=UInt(0, width=16))

      when(wBufferFillValidReadBRAM){
        printf("Output from bramRead: %b\n", bramReadPort.rsp.readData)
        printf("LastCycleCol... : %d, remainMask: %b, writePosition: %d\n", lastCycleColBitInWord, remainMask, wBufferFillWritePosition)
        val readAndFilteredFromBRAM = UInt((bramReadPort.rsp.readData >> lastCycleColBitInWord) & remainMask, width=16)
        printf("Filtered value from BRAM: %b\n", readAndFilteredFromBRAM)
        temporaryBuffer := UInt(temporaryBuffer | (readAndFilteredFromBRAM << wBufferFillWritePosition), width=256)
        printf("Temp after or %d\n", UInt(temporaryBuffer | (readAndFilteredFromBRAM << wBufferFillWritePosition), width=256))
        wBufferFillWritePosition := wBufferFillWritePosition + lastStride
      }

      when(wBufferFillValidReadBRAM && (wBufferFillNumRowsRead === io.windowSize)){
        printf("tempBuffer readily filled: %b\n", temporaryBuffer)
        state := s_finished
          //state := s_transfer_window_size_buffer
      }.otherwise{
        when(wBufferFillNumBitsReadOnRow + UInt(wordSizeInBits) - wBufferFillReadColumnBitInWord < io.windowSize){
          wBufferFillReadColumnWord := wBufferFillReadColumnWord + UInt(1)
          wBufferFillReadColumnBitInWord := UInt(0)
          wBufferFillNumBitsReadOnRow := wBufferFillNumBitsReadOnRow + UInt(wordSizeInBits) - wBufferFillReadColumnBitInWord
          lastStride := UInt(wordSizeInBits) - wBufferFillReadColumnBitInWord
        }.otherwise{
          printf("Finished row, next one!\n")
          wBufferFillReadColumnWord := currInputColWord
          wBufferFillReadColumnBitInWord := currInputColBitInWord
          wBufferFillNumBitsReadOnRow := UInt(0)
          wBufferFillNumRowsRead := wBufferFillNumRowsRead + UInt(1)
          lastStride := io.windowSize
          when(wBufferFillReadRow === io.windowSize - UInt(1)){
            wBufferFillReadRow := UInt(0)
          }.otherwise{
            wBufferFillReadRow := wBufferFillReadRow + UInt(1)
          }
        }
        lastCycleColBitInWord := wBufferFillReadColumnBitInWord
        remainMask := (UInt(1) << (io.windowSize - wBufferFillNumBitsReadOnRow)) - UInt(1)

        bramReadPort.req.addr := wBufferFillReadRow * inputWordsPerRow + wBufferFillReadColumnWord
        wBufferFillValidReadBRAM := Bool(true)
      }
    }

    is(s_transfer_window_size_buffer){
      bufferToWrite := bufferToWrite | ((temporaryBuffer >> bufferTransferReadCurrCol) & ((UInt(1) << (io.windowSize - bufferTransferReadCurrCol)) - UInt(1)) << bufferTransferWriteCurrCol)
      when(bufferTransferWriteCurrCol + io.windowSize >= UInt(wordSizeInBits)){
        bufferTransferReadCurrCol := bufferTransferWriteCurrCol + io.windowSize - UInt(wordSizeInBits)
        bufferTransferWriteCurrCol := UInt(0)
        state := s_write_buffer
      }.otherwise{
        when(bufferTransferWriteCurrCol + io.windowSize === UInt(wordSizeInBits)){
          bufferTransferWriteCurrCol := UInt(0)
          bufferTransferReadCurrCol := UInt(0)
        }
        bufferTransferWriteCurrCol := bufferTransferWriteCurrCol + io.windowSize
        state := s_fill_window_size_buffer
      }
    }

    is(s_write_buffer){
      when(!writer.active && !writerOnLastCycle){
        writer.baseAddr := currOutputBitplane * outputBitplaneSizeInBytes + ((currOutputRow * outputRowSizeInWords) << wordByteExponent)
      }

      when(writer.in.ready){ // Successfully queued chunk
        when(bufferTransferReadCurrCol === UInt(0)){
          // Prepare BRAM filling?
          state := s_fill_bram
        }.otherwise{
          // More preparation?
          state := s_transfer_window_size_buffer
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
