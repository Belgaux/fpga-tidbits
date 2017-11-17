package fpgatidbits.rosetta

import Chisel._

import fpgatidbits.dma._
import fpgatidbits.ocm._
import fpgatidbits.streams._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.rosetta._

// Expects input of form channel/bits/rows/columns, with every row padded to wordsize bits
class SlidingWindow(p: PlatformWrapperParams, _wordSizeInBits:Int) extends Module {
  val wordSizeInBits = _wordSizeInBits
  val wordSizeInBytes = wordSizeInBits/8
  val numMemPorts = 2

  val io = new Bundle {
    val numCols = UInt(INPUT, width=16)
    val numRows = UInt(INPUT, width=16)
    val numBits = UInt(INPUT, width=16)
    val numChannels = UInt(INPUT, width=16)
    val windowSize = UInt(INPUT, width=8)
    val strideExponent = UInt(INPUT, width=3)
    val addrImage =  UInt(INPUT, width=wordSizeInBits)
    val addrResult = UInt(INPUT, width=wordSizeInBits)
    val start = Bool(INPUT)

    val readerIF = new StreamReaderIF(wordSizeInBits, p.toMemReqParams()).flip
    val writerIF = new StreamWriterIF(wordSizeInBits, p.toMemReqParams()).flip
    val finished = Bool(OUTPUT)

    // Debug:
    val checkAddrBRAM = UInt(INPUT, width=32)
    val debugOutput = UInt(OUTPUT, width=wordSizeInBits)
  }

  val wordBitExponent = log2Up(wordSizeInBits)
  val wordByteExponent = wordBitExponent - 3

  val reader = io.readerIF
  val writer = io.writerIF

  // Set initial state
  val s_idle :: s_fill_bram_start :: s_fill_bram_setup_reader :: s_fill_bram_read_row :: s_fill_bram_row_finished :: s_fill_bram_all_rows_finished :: s_fill_window_size_buffer_configure_bram :: s_fill_window_size_buffer_fill :: s_write_buffer_enqueue :: s_write_buffer_loopback :: s_write_buffer_finished_channel :: s_wait_for_writer_finish :: s_finished :: Nil = Enum(UInt(), 13) 
  val state = Reg(init=UInt(s_idle))

  reader.baseAddr := UInt(0)
  reader.byteCount := UInt(0)
  reader.out.ready := Bool(false)

  writer.start := Bool(true)
  writer.baseAddr := UInt(0)
  writer.byteCount := UInt(0)
  writer.in.valid := Bool(false)
  writer.in.bits := UInt(0)

  val readerEnableReg = Reg(init=Bool(false))
  readerEnableReg := Bool(false)
  reader.start := readerEnableReg

  val writerActiveLastCycle = Reg(init=Bool(false))
  val writerFinishedLastCycle = Reg(init=Bool(false))
  val timesWriterFinished = Reg(init=UInt(0, width=32))

  writerActiveLastCycle := writer.active
  writerFinishedLastCycle := writer.finished
  when(writerActiveLastCycle){
    writer.start := Bool(true)
  }.otherwise{
    writer.start := Bool(false)
  }

  when(writerFinishedLastCycle){ // Make sure writer resets properly between writes
    timesWriterFinished := timesWriterFinished + UInt(1)
    writer.start := Bool(false)
  }

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

  val stride = UInt(1) << io.strideExponent

  val currInputRow = Reg(init=UInt(width=16))
  val currInputCol = Reg(init=UInt(width=32))
  val currInputBitplane = Reg(init=UInt(width=16))
  val currInputChannel = Reg(init=UInt(width=16))

  val currOutputRow = Reg(init=UInt(0, width=32))
  val currTempBufferOutputBit = Reg(init=UInt(0))

  val bramInputFillingRow = Reg(init=UInt(0, width=16))
  val bramInputFillingColWord = Reg(init=UInt(0, width=16))
  val bramOutputFillingRow = Reg(init=UInt(0, width=16))
  val currStartBRAMRow = Reg(init=UInt(0, width=16))
  val numBRAMRowsToFill = Reg(init=UInt(0, width=16))

  val wBufferFillReadColumnWord = Reg(init=UInt(0, width=32))
  val wBufferFillReadColumnBitInWord = Reg(init=UInt(0, width=32))
  val wBufferFillReadRow = Reg(init=UInt(0, width=16))
  val wBufferFillNumRowsRead = Reg(init=UInt(0, width=16))
  val wBufferFillWritePosition = Reg(init=UInt(0, width=8))
  val wBufferFillValidReadBRAM = Reg(init=Bool(false))
  val wBufferFillNumBitsReadOnRow = Reg(init=UInt(0, width=16))
  val wBufferFillRemainMask = Reg(init=UInt(0, width=16))
  val wBufferFillLastCycleColBitInWord = Reg(init=UInt(0, width=16))
  val wBufferFillLastStride = Reg(init=UInt(0, width=16))

  val temporaryBuffer = Reg(init=UInt(width=128)) // Used in moving from BRAM to writebuffer - 128 bits is enough for window size 11x11

  wBufferFillValidReadBRAM := Bool(false) // Should only be true if explicitly told to. Best to be safe

  val currInputColWord = currInputCol >> wordBitExponent
  val currInputColBitInWord = currInputCol & UInt(wordSizeInBits - 1)

  // Convenient constants
  val inputWordsPerRow = ((io.numCols + UInt(wordSizeInBits - 1)) >> wordBitExponent)
  val inputBytesPerRow = (inputWordsPerRow << wordByteExponent)

  val inputBitplaneSizeInBytes = Reg(init=UInt(0, width=16))
  val inputChannelSizeInBytes = Reg(init=UInt(0, width=32))
  val inputBitsChannelsOffset = Reg(init=UInt(0, width=32))

  val windowSizeSquared = Reg(init=UInt(0, width=16))
  val paddedWindowSizeSquaredSizeInBits = Reg(init=UInt(0))
  val paddedWindowSizeSquaredSizeInBytes = paddedWindowSizeSquaredSizeInBits >> 3
  val outputRowSizeInWords = Reg(next=(Reg(next=(windowSizeSquared + UInt(wordSizeInBits - 1) >> wordBitExponent)) *io.numChannels))
  val outputRowSizeInBytes = outputRowSizeInWords << wordByteExponent
  val outputBitplaneSizeInBytes = Reg(init=UInt(0, width=32))
  val outputNumRowsPerBitplane = Reg(init = UInt(0, width = 32))

  val writerWaitForNumFinished = Reg(init=UInt(0, width=32))

  val windowSizeMask = Reg(init = UInt(0, width=16))

  val currOutputBitplaneOffset = Reg(next=outputBitplaneSizeInBytes * currInputBitplane)
  val currOutputRowOffset = Reg(next = outputRowSizeInBytes * currOutputRow)
  val currOutputChannelOffset = Reg(next=currInputChannel * paddedWindowSizeSquaredSizeInBytes)

  inputBitplaneSizeInBytes := io.numRows * inputBytesPerRow
  inputChannelSizeInBytes := io.numBits * inputBitplaneSizeInBytes
  inputBitsChannelsOffset := inputChannelSizeInBytes * currInputChannel + inputBitplaneSizeInBytes * currInputBitplane

  windowSizeSquared := io.windowSize * io.windowSize
  paddedWindowSizeSquaredSizeInBits := ((windowSizeSquared + UInt(wordSizeInBits - 1)) >> wordBitExponent) << wordBitExponent
  outputNumRowsPerBitplane := Reg(next= (Reg(next=(((io.numRows - io.windowSize) >> io.strideExponent) + UInt(1))) * Reg(next=(((io.numCols - io.windowSize) >> io.strideExponent) + UInt(1)))))
  outputBitplaneSizeInBytes := (outputNumRowsPerBitplane * outputRowSizeInBytes)

  windowSizeMask := (UInt(1) << io.windowSize) - UInt(1)

  writer.byteCount := paddedWindowSizeSquaredSizeInBytes

  val readerInputRow = Reg(next = (currInputRow + io.windowSize - numBRAMRowsToFill + bramInputFillingRow))
  val readerInputRowOffset = Reg(next = (readerInputRow * inputBytesPerRow))
  val bramOutputFillingRowOffset = Reg(next = (bramOutputFillingRow * inputWordsPerRow))

  switch (state){
    is(s_idle){
      when(io.start){
        state := s_fill_bram_start
        numBRAMRowsToFill := io.windowSize
        bramOutputFillingRow := UInt(0)
        bramInputFillingColWord := UInt(0)
        bramInputFillingRow := UInt(0)
        currInputCol := UInt(0)
        currInputRow := UInt(0)
        currInputBitplane := UInt(0)
        currInputChannel := UInt(0)
        currStartBRAMRow := UInt(0)
        currOutputRow := UInt(0)
      }
    }

    is(s_fill_bram_start){
      val counter = Reg(init=UInt(0, width=4))
      counter := counter + UInt(1)
      when(counter === UInt(2)){
        counter := UInt(0)
        state := s_fill_bram_setup_reader
      }
    }

    is(s_fill_bram_setup_reader){
      reader.baseAddr := io.addrImage + inputBitsChannelsOffset + readerInputRowOffset
      reader.byteCount := inputWordsPerRow << wordByteExponent
      readerEnableReg := Bool(true)
      reader.out.ready := Bool(true)
      bramWritePort.req.addr :=  bramOutputFillingRowOffset + bramInputFillingColWord
      bramWritePort.req.writeEn := reader.out.valid
      bramWritePort.req.writeData := reader.out.bits

      when(reader.out.valid){ // Another word is transferred
        state := s_fill_bram_read_row
      }
    }

    is(s_fill_bram_read_row){
        reader.start := Bool(false)
        when(bramInputFillingColWord === inputWordsPerRow - UInt(1)){ // Finished reading in row
          state := s_fill_bram_row_finished
        }.otherwise{
          bramInputFillingColWord := bramInputFillingColWord + UInt(1)
          state := s_fill_bram_start
        }
    }

    is(s_fill_bram_row_finished){
      bramInputFillingColWord := UInt(0)

      when(bramOutputFillingRow === io.windowSize - UInt(1)){
        bramOutputFillingRow := UInt(0)
      }.otherwise{
        bramOutputFillingRow := bramOutputFillingRow + UInt(1)
      }

      state := s_fill_bram_start

      when(bramInputFillingRow === numBRAMRowsToFill - UInt(1)){ // Finished all rows that should be read
        state := s_fill_bram_all_rows_finished
      }.otherwise{
        bramInputFillingRow := bramInputFillingRow + UInt(1)

      }
    }

    is(s_fill_bram_all_rows_finished){
      bramInputFillingRow := UInt(0)
      currStartBRAMRow := bramOutputFillingRow
      wBufferFillReadRow := bramOutputFillingRow
      
      wBufferFillWritePosition := UInt(0)
      wBufferFillReadColumnWord := currInputColWord
      wBufferFillReadColumnBitInWord := currInputColBitInWord
      wBufferFillValidReadBRAM := Bool(false)
      temporaryBuffer := UInt(0)
      wBufferFillNumBitsReadOnRow := UInt(0)
      wBufferFillNumRowsRead := UInt(0)

      state := s_fill_window_size_buffer_configure_bram
    }

    is(s_fill_window_size_buffer_configure_bram){
      when(wBufferFillNumBitsReadOnRow + UInt(wordSizeInBits) - wBufferFillReadColumnBitInWord < io.windowSize){
        wBufferFillReadColumnWord := wBufferFillReadColumnWord + UInt(1)
        wBufferFillReadColumnBitInWord := UInt(0)
        wBufferFillNumBitsReadOnRow := wBufferFillNumBitsReadOnRow + UInt(wordSizeInBits) - wBufferFillReadColumnBitInWord
        val currStride = UInt(wordSizeInBits) - wBufferFillReadColumnBitInWord
        wBufferFillLastStride := currStride
        wBufferFillRemainMask := (UInt(1,width=16) << currStride) - UInt(1,width=16)
      }.otherwise{
        wBufferFillReadColumnWord := currInputColWord
        wBufferFillReadColumnBitInWord := currInputColBitInWord
        wBufferFillNumBitsReadOnRow := UInt(0)
        wBufferFillNumRowsRead := wBufferFillNumRowsRead + UInt(1)
        wBufferFillLastStride := io.windowSize - wBufferFillNumBitsReadOnRow
        when(wBufferFillReadRow === io.windowSize - UInt(1)){
          wBufferFillReadRow := UInt(0)
        }.otherwise{
          wBufferFillReadRow := wBufferFillReadRow + UInt(1)
        }

        wBufferFillRemainMask := (UInt(1) << (io.windowSize - wBufferFillNumBitsReadOnRow)) - UInt(1)
      }
      wBufferFillLastCycleColBitInWord := wBufferFillReadColumnBitInWord

      bramReadPort.req.addr := wBufferFillReadRow * inputWordsPerRow + wBufferFillReadColumnWord
      wBufferFillValidReadBRAM := Bool(true)

      state := s_fill_window_size_buffer_fill
    }

    is(s_fill_window_size_buffer_fill){
      val readAndFilteredFromBRAM = UInt((bramReadPort.rsp.readData >> wBufferFillLastCycleColBitInWord) & wBufferFillRemainMask, width=16)
      val newTemp = (temporaryBuffer | (readAndFilteredFromBRAM << wBufferFillWritePosition))
      temporaryBuffer := newTemp
      wBufferFillWritePosition := wBufferFillWritePosition + wBufferFillLastStride

      when(wBufferFillNumRowsRead === io.windowSize){
        state := s_write_buffer_enqueue
      }.otherwise{
        state := s_fill_window_size_buffer_configure_bram
      }
    }

    is(s_write_buffer_enqueue){
      when(timesWriterFinished === writerWaitForNumFinished){
        writer.baseAddr := io.addrResult + currOutputBitplaneOffset + currOutputRowOffset + currOutputChannelOffset
        writer.start := Bool(true)
        writer.byteCount := paddedWindowSizeSquaredSizeInBytes  // From bits to bytes
        writer.in.valid := Bool(true)
        writer.in.bits := temporaryBuffer >> currTempBufferOutputBit

        when(writer.in.ready){ // Successfully queued chunk
          state := s_write_buffer_loopback
        }
      }
    }

    is(s_write_buffer_loopback){
      when(currTempBufferOutputBit === paddedWindowSizeSquaredSizeInBits - UInt(wordSizeInBits)){ // Finished with whole temp buffer
        writerWaitForNumFinished := writerWaitForNumFinished + UInt(1)
        currTempBufferOutputBit := UInt(0)
        temporaryBuffer := UInt(0)

        wBufferFillNumBitsReadOnRow := UInt(0)
        wBufferFillNumRowsRead := UInt(0)
        wBufferFillWritePosition := UInt(0)
        wBufferFillReadRow := currStartBRAMRow
        wBufferFillValidReadBRAM := Bool(false)
        state := s_fill_window_size_buffer_configure_bram
        when(currInputCol + io.windowSize === io.numCols){
          currInputCol := UInt(0)
          wBufferFillReadColumnBitInWord := UInt(0)
          wBufferFillReadColumnWord := UInt(0)

          
          state := s_fill_bram_start
          when(currInputRow + io.windowSize === io.numRows){
            numBRAMRowsToFill := io.windowSize
            currInputRow := UInt(0)
          }.otherwise{
            numBRAMRowsToFill := stride
            currInputRow := currInputRow + stride
          }
        }.otherwise{
          wBufferFillReadColumnBitInWord := (currInputCol + stride) & UInt(wordSizeInBits - 1)
          wBufferFillReadColumnWord := (currInputCol + stride) >> wordBitExponent
          currInputCol := currInputCol + stride
        }

        when(currOutputRow === outputNumRowsPerBitplane - UInt(1)){ // Finished with a channel batch
          state := s_write_buffer_finished_channel
        }.otherwise{
          currOutputRow := currOutputRow + UInt(1)
        }
      }.otherwise{
        currTempBufferOutputBit := currTempBufferOutputBit + UInt(wordSizeInBits)
        state := s_write_buffer_enqueue
      }
    }

    is(s_write_buffer_finished_channel){
      numBRAMRowsToFill := io.windowSize
      currOutputRow := UInt(0)

      state := s_fill_bram_start
      when(currInputChannel === io.numChannels - UInt(1)){ // Finished with all channels of a bitplane
        currInputChannel := UInt(0)
        when(currInputBitplane === io.numBits - UInt(1)){ // Finished all bitplanes
          state := s_wait_for_writer_finish
        }.otherwise{
          currInputBitplane := currInputBitplane + UInt(1)
        }
      }.otherwise{
        currInputChannel := currInputChannel + UInt(1)
      }
    }

    is(s_wait_for_writer_finish){
      writer.start := Bool(true)
      when(writer.finished){
        state := s_finished
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
