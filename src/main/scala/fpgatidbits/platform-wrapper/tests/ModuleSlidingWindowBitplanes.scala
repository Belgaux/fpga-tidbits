package fpgatidbits.Testbenches

import Chisel._

import fpgatidbits.dma._
import fpgatidbits.ocm._
import fpgatidbits.streams._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.rosetta._


// Expects input of form channel/bits/rows/columns, with every row padded to wordsize bits
class ModuleSlidingWindowBitplanes(p: PlatformWrapperParams, _wordSizeInBits:Int) extends Module {
  val wordSizeInBits = _wordSizeInBits
  val wordSizeInBytes = wordSizeInBits/8
  val numMemPorts = 2

  val io = new Bundle {
    val numCols = UInt(INPUT, width=16)
    val numRows = UInt(INPUT, width=16)
    val numBits = UInt(INPUT, width=16)
    val numChannels = UInt(INPUT, width=16)
    val windowSize = UInt(INPUT, width=8)
    val stride = UInt(INPUT, width=4) // Keep low for division purposes
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
  val s_idle :: s_fill_bram :: s_fill_window_size_buffer :: s_write_buffer :: s_wait_for_writer_finish :: s_finished :: Nil = Enum(UInt(), 6) 
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
  val outputRowSizeInWords  = ((windowSizeSquared + UInt(wordSizeInBits - 1)) >> wordBitExponent) * io.numChannels
  val outputRowSizeInBytes = outputRowSizeInWords << wordByteExponent
  val outputBitplaneSizeInBytes = Reg(init=UInt(0, width=32))
  val outputNumRowsPerBitplane = Reg(init = UInt(0, width = 32))

  val writerWaitForNumFinished = Reg(init=UInt(0, width=32))

  val windowSizeMask = Reg(init = UInt(0, width=16))

  inputBitplaneSizeInBytes := io.numRows * inputBytesPerRow
  inputChannelSizeInBytes := io.numBits * inputBitplaneSizeInBytes
  inputBitsChannelsOffset := inputChannelSizeInBytes * currInputChannel + inputBitplaneSizeInBytes * currInputBitplane

  windowSizeSquared := io.windowSize * io.windowSize
  paddedWindowSizeSquaredSizeInBits := ((windowSizeSquared + UInt(wordSizeInBits - 1)) >> wordBitExponent) << wordBitExponent
  outputNumRowsPerBitplane := ((io.numRows - io.windowSize)/io.stride + UInt(1)) * ((io.numCols - io.windowSize)/io.stride + UInt(1))
  outputBitplaneSizeInBytes := (outputNumRowsPerBitplane * outputRowSizeInWords) << wordByteExponent

  windowSizeMask := (UInt(1) << io.windowSize) - UInt(1)

  writer.byteCount := paddedWindowSizeSquaredSizeInBytes

  //printf("Tick\n")

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
        currOutputRow := UInt(0)
      }
    }

    is(s_fill_bram){
      reader.baseAddr := io.addrImage + inputBitsChannelsOffset + (currInputRow + io.windowSize - numBRAMRowsToFill + bramInputFillingRow) * inputBytesPerRow
      reader.byteCount := inputWordsPerRow << wordByteExponent
      readerEnableReg := Bool(true)
      reader.out.ready := Bool(true)
      bramWritePort.req.addr :=  bramOutputFillingRow * inputWordsPerRow + bramInputFillingColWord
      bramWritePort.req.writeEn := reader.out.valid
      bramWritePort.req.writeData := reader.out.bits

      when(reader.out.valid){ // Another word is transferred
        //printf("Writing %b to address %d in BRAM\n", bramWritePort.req.writeData, bramWritePort.req.addr)
        reader.start := Bool(false)
        when(bramInputFillingColWord === inputWordsPerRow - UInt(1)){ // Finished reading in row
          bramInputFillingColWord := UInt(0)

          when(bramOutputFillingRow === io.windowSize - UInt(1)){
            bramOutputFillingRow := UInt(0)
          }.otherwise{
            bramOutputFillingRow := bramOutputFillingRow + UInt(1)
          }

          when(bramInputFillingRow === numBRAMRowsToFill - UInt(1)){ // Finished all rows that should be read
            bramInputFillingRow := UInt(0)
            when(bramOutputFillingRow === io.windowSize - UInt(1)){ // Must start to fill BRAM from top next time
              currStartBRAMRow := UInt(0)
              wBufferFillReadRow := UInt(0)
            }.otherwise{
              currStartBRAMRow := bramOutputFillingRow + UInt(1)
              wBufferFillReadRow := bramOutputFillingRow + UInt(1)
            }
            wBufferFillWritePosition := UInt(0)
            wBufferFillReadColumnWord := currInputColWord
            wBufferFillReadColumnBitInWord := currInputColBitInWord
            wBufferFillValidReadBRAM := Bool(false)
            temporaryBuffer := UInt(0)
            wBufferFillNumBitsReadOnRow := UInt(0)
            wBufferFillNumRowsRead := UInt(0)
            //printf("Filled BRAM, currInputChannel = %d\n", currInputChannel)
            state := s_fill_window_size_buffer
          }.otherwise{
            bramInputFillingRow := bramInputFillingRow + UInt(1)

          }
        }.otherwise{
          bramInputFillingColWord := bramInputFillingColWord + UInt(1)
        }
      }
    }

    is(s_fill_window_size_buffer){
      val remainMask = Reg(init=UInt(0, width=16))
      val lastCycleColBitInWord = Reg(init=UInt(0, width=16))
      val lastStride = Reg(init=UInt(0, width=16))

      when(wBufferFillValidReadBRAM){
        val readAndFilteredFromBRAM = UInt((bramReadPort.rsp.readData >> lastCycleColBitInWord) & remainMask, width=16)

        val newTemp = (temporaryBuffer | (readAndFilteredFromBRAM << wBufferFillWritePosition))
        temporaryBuffer := newTemp
        wBufferFillWritePosition := wBufferFillWritePosition + lastStride
      }

      when(wBufferFillValidReadBRAM && (wBufferFillNumRowsRead === io.windowSize)){
        state := s_write_buffer
      }.otherwise{
        when(wBufferFillNumBitsReadOnRow + UInt(wordSizeInBits) - wBufferFillReadColumnBitInWord < io.windowSize){
          wBufferFillReadColumnWord := wBufferFillReadColumnWord + UInt(1)
          wBufferFillReadColumnBitInWord := UInt(0)
          wBufferFillNumBitsReadOnRow := wBufferFillNumBitsReadOnRow + UInt(wordSizeInBits) - wBufferFillReadColumnBitInWord
          val currStride = UInt(wordSizeInBits) - wBufferFillReadColumnBitInWord
          lastStride := currStride
          remainMask := (UInt(1,width=16) << currStride) - UInt(1,width=16)
        }.otherwise{
          wBufferFillReadColumnWord := currInputColWord
          wBufferFillReadColumnBitInWord := currInputColBitInWord
          wBufferFillNumBitsReadOnRow := UInt(0)
          wBufferFillNumRowsRead := wBufferFillNumRowsRead + UInt(1)
          lastStride := io.windowSize - wBufferFillNumBitsReadOnRow
          when(wBufferFillReadRow === io.windowSize - UInt(1)){
            wBufferFillReadRow := UInt(0)
          }.otherwise{
            wBufferFillReadRow := wBufferFillReadRow + UInt(1)
          }

          remainMask := (UInt(1) << (io.windowSize - wBufferFillNumBitsReadOnRow)) - UInt(1)
        }
        lastCycleColBitInWord := wBufferFillReadColumnBitInWord

        bramReadPort.req.addr := wBufferFillReadRow * inputWordsPerRow + wBufferFillReadColumnWord
        wBufferFillValidReadBRAM := Bool(true)
      }
    }

    is(s_write_buffer){
      when(timesWriterFinished === writerWaitForNumFinished){
        writer.baseAddr := io.addrResult + outputBitplaneSizeInBytes * currInputBitplane + outputRowSizeInBytes * currOutputRow + currInputChannel * (paddedWindowSizeSquaredSizeInBytes)
        writer.start := Bool(true)
        writer.byteCount := paddedWindowSizeSquaredSizeInBytes  // From bits to bytes
        writer.in.valid := Bool(true)
        writer.in.bits := temporaryBuffer >> currTempBufferOutputBit

        when(writer.in.ready){ // Successfully queued chunk
          printf("Queued chunk %d\n", timesWriterFinished)
          when(currTempBufferOutputBit === paddedWindowSizeSquaredSizeInBits - UInt(wordSizeInBits)){ // Finished with whole temp buffer
            writerWaitForNumFinished := writerWaitForNumFinished + UInt(1)
            currTempBufferOutputBit := UInt(0)
            temporaryBuffer := UInt(0)

            wBufferFillNumBitsReadOnRow := UInt(0)
            wBufferFillNumRowsRead := UInt(0)
            wBufferFillWritePosition := UInt(0)
            wBufferFillReadRow := currStartBRAMRow
            wBufferFillValidReadBRAM := Bool(false)

            state := s_fill_window_size_buffer
            when(currOutputRow === outputNumRowsPerBitplane - UInt(1)){ // Finished with a channel batch
              numBRAMRowsToFill := io.windowSize
              currOutputRow := UInt(0)
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
            }.otherwise{
              currOutputRow := currOutputRow + UInt(1)
            }

            val done = writer.in.ready && (currTempBufferOutputBit === paddedWindowSizeSquaredSizeInBits - UInt(wordSizeInBits)) && (currOutputRow === outputNumRowsPerBitplane - UInt(1)) && (currInputChannel === io.numChannels - UInt(1)) && (currInputBitplane === io.numBits - UInt(1))

            when(done){
              state := s_wait_for_writer_finish
            }.elsewhen(currInputCol + io.windowSize === io.numCols){
              currInputCol := UInt(0)
              wBufferFillReadColumnBitInWord := UInt(0)
              wBufferFillReadColumnWord := UInt(0)

              state := s_fill_bram
              when(currInputRow + io.windowSize === io.numRows){
                numBRAMRowsToFill := io.windowSize
                currInputRow := UInt(0)
              }.otherwise{
                numBRAMRowsToFill := io.stride
                currInputRow := currInputRow + io.stride
              }
            }.otherwise{
              wBufferFillReadColumnBitInWord := (currInputCol + io.stride) & UInt(wordSizeInBits - 1)
              wBufferFillReadColumnWord := (currInputCol + io.stride) >> wordBitExponent
              currInputCol := currInputCol + io.stride
            }
          }.otherwise{
            currTempBufferOutputBit := currTempBufferOutputBit + UInt(wordSizeInBits)
          }

        }
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
