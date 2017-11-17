package fpgatidbits.Testbenches

import Chisel._

import fpgatidbits.dma._
import fpgatidbits.ocm._
import fpgatidbits.streams._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.rosetta._


// Ties together the sliding window module and the matrix multiplication module
// Presumes input image is of form channels/bitplanes/rows/columns with each row padded to 64 bits
class Convolution(p: PlatformWrapperParams, _wordSizeInBits:Int) extends Module {
  val wordSizeInBits = _wordSizeInBits
  val wordSizeInBytes = wordSizeInBits/8

  val wordBitExponent = log2Up(wordSizeInBits)
  val wordByteExponent = wordBitExponent - 3

  val numMemPorts = 3

  val io = new Bundle {
    val imageAddr = UInt(INPUT, width=64)
    val filterAddr = UInt(INPUT, width=64)
    val outputAddr = UInt(INPUT, width=64)
    val tempAddr = UInt(INPUT, width=64)

    val imageWidth = UInt(INPUT, width=32)
    val imageHeight = UInt(INPUT, width=32)
    val imageNumBits = UInt(INPUT, width=4)
    val imageNumChannels = UInt(INPUT, width=16)

    val strideExponent = UInt(INPUT, width=3)
    val windowSize = UInt(INPUT, width=4)
    val numOutputChannels = UInt(INPUT, width=16)

    val filtersNumBits = UInt(INPUT, width=4)

    val start = Bool(INPUT)
    val finishedWithSlidingWindow = Bool(OUTPUT)

    val reader1IF = new StreamReaderIF(wordSizeInBits, p.toMemReqParams()).flip
    val reader2IF = new StreamReaderIF(wordSizeInBits, p.toMemReqParams()).flip
    val writerIF = new StreamWriterIF(wordSizeInBits, p.toMemReqParams()).flip

    val finished = Bool(OUTPUT)
  }

  val reader1 = io.reader1IF
  val reader2 = io.reader2IF

  val writer = io.writerIF

  reader1.baseAddr := UInt(0)
  reader1.byteCount := UInt(0)
  reader1.out.ready := Bool(false)
  reader1.start := Bool(false)

  reader2.baseAddr := UInt(0)
  reader2.byteCount := UInt(0)
  reader2.out.ready := Bool(false)
  reader2.start := Bool(false)

  writer.baseAddr := UInt(0)
  writer.byteCount := UInt(0)
  writer.in.valid := Bool(false)
  writer.in.bits := UInt(0)
  writer.start := Bool(false)

  //printf("Writer start: %d, baseAddr: %d, byteCount: %d, in.valid: %d, in.bits: %d, in.ready: %d\n",
  //  writer.start, writer.baseAddr, writer.byteCount, writer.in.valid, writer.in.bits, writer.in.ready)

  // Handy expressions constant throughout a run

  val windowSizeSquared = Reg(init=UInt(0, width=8))
  val slidedWindowNumWidth = Reg(init=UInt(0, width=32))
  val slidedWindowNumHeight = Reg(init=UInt(0, width=32))
  val slidedWindowNumRowsInBitplane = Reg(init=UInt(0, width=32))
  val slidedWindowSizeInWords = Reg(init=UInt(0, width=32))
  val slidedWindowRowSizeInWords = Reg(init=UInt(0, width=32))
  val slidedWindowBitplaneSizeInWords = Reg(init=UInt(0, width=32))
  val slidedWindowTotalSizeInBytes = Reg(init=UInt(0, width=32))

  val resultChannelSizeInBytes = Reg(init=UInt(0, width=32))
  val resultTotalSizeInBytes = Reg(init=UInt(0, width=32))

  windowSizeSquared := io.windowSize * io.windowSize
  slidedWindowNumWidth := ((io.imageWidth - io.windowSize) >> io.strideExponent) + UInt(1)
  slidedWindowNumHeight := ((io.imageHeight - io.windowSize) >> io.strideExponent) + UInt(1)
  slidedWindowNumRowsInBitplane := slidedWindowNumHeight * slidedWindowNumWidth
  slidedWindowSizeInWords := ((windowSizeSquared + UInt(wordSizeInBits - 1)) >> wordBitExponent)
  slidedWindowRowSizeInWords := slidedWindowSizeInWords * io.imageNumChannels
  slidedWindowBitplaneSizeInWords := slidedWindowRowSizeInWords * slidedWindowNumRowsInBitplane
  slidedWindowTotalSizeInBytes := (io.imageNumBits * slidedWindowBitplaneSizeInWords) << wordByteExponent

  resultChannelSizeInBytes := slidedWindowNumRowsInBitplane << wordByteExponent // Assume one word per output element
  resultTotalSizeInBytes := resultChannelSizeInBytes * io.numOutputChannels

  val s_idle :: s_running_sliding_window :: s_running_multiplication :: s_finished :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_idle)

  io.finished := Bool(false)
  io.finishedWithSlidingWindow := Bool(false)

  val windowSlider = Module(new SlidingWindow(p, wordSizeInBits)).io

  windowSlider.numCols := io.imageWidth
  windowSlider.numRows := io.imageHeight
  windowSlider.numChannels := io.imageNumChannels
  windowSlider.numBits := io.imageNumBits
  windowSlider.strideExponent := io.strideExponent
  windowSlider.windowSize := io.windowSize
  windowSlider.addrImage := io.imageAddr
  windowSlider.addrResult := io.tempAddr
  windowSlider.start := Bool(false)


  val multiplier = Module(new BitserialGEMM(wordSizeInBits, p)).io
  multiplier.lhs_addr := io.filterAddr
  multiplier.rhs_addr := io.tempAddr
  multiplier.res_addr := io.outputAddr
  multiplier.num_chn := UInt(1)

  multiplier.lhs_rows := io.numOutputChannels 
  multiplier.lhs_cols := slidedWindowRowSizeInWords
  multiplier.lhs_bits := io.filtersNumBits
  multiplier.lhs_issigned := Bool(true) // Is this ok?

  // Double check that rows is rows in memory, and not of the transposed of what's in memory
  multiplier.rhs_rows := slidedWindowNumRowsInBitplane
  multiplier.rhs_cols := slidedWindowRowSizeInWords
  multiplier.rhs_bits := io.imageNumBits
  multiplier.rhs_issigned := Bool(true) // Probably

  multiplier.start := Bool(false)

  // Need to specify defaults (cannot be the best way?)
  windowSlider.readerIF.finished := Bool(false)
  windowSlider.readerIF.out.bits := UInt(0)
  windowSlider.readerIF.out.valid := Bool(false)

  windowSlider.writerIF.finished := Bool(false)
  windowSlider.writerIF.in.ready := Bool(false)
  windowSlider.writerIF.active := Bool(false)

  multiplier.lhs_reader.finished := Bool(false)
  multiplier.lhs_reader.out.bits := UInt(0)
  multiplier.lhs_reader.out.valid := Bool(false)

  multiplier.rhs_reader.finished := Bool(false)
  multiplier.rhs_reader.out.bits := UInt(0)
  multiplier.rhs_reader.out.valid := Bool(false)

  multiplier.writer.finished := Bool(false)
  multiplier.writer.in.ready := Bool(false)
  multiplier.writer.active := Bool(false)


  switch(state) {
    is (s_idle) {
      when(io.start){
        state := s_running_sliding_window
      }
    }

    is (s_running_sliding_window) {
      windowSlider.start := Bool(true)

      windowSlider.readerIF.out.bits := reader1.out.bits
      windowSlider.readerIF.out.valid := reader1.out.valid
      reader1.out.ready := windowSlider.readerIF.out.ready

      reader1.byteCount := windowSlider.readerIF.byteCount
      reader1.baseAddr := windowSlider.readerIF.baseAddr
      reader1.start := windowSlider.readerIF.start
      windowSlider.readerIF.finished := reader1.finished
      //reader1 <> windowSlider.readerIF // Apparently, this does not work

      writer.in.bits := windowSlider.writerIF.in.bits
      writer.in.valid := windowSlider.writerIF.in.valid
      windowSlider.writerIF.in.ready := writer.in.ready

      writer.byteCount := windowSlider.writerIF.byteCount
      writer.baseAddr := windowSlider.writerIF.baseAddr
      writer.start := windowSlider.writerIF.start
      windowSlider.writerIF.finished := writer.finished
      windowSlider.writerIF.active := writer.active
      // writer <> windowSlider.writerIF // But why??

      when(windowSlider.finished){
        io.finishedWithSlidingWindow := Bool(true)
        writer.start := Bool(false)
        reader1.start := Bool(false)
        reader2.start := Bool(false)
        state := s_running_multiplication
      }
    }

    is (s_running_multiplication) {

      io.finishedWithSlidingWindow := Bool(true)
      multiplier.start := Bool(true)

      multiplier.lhs_reader.out.bits := reader1.out.bits
      multiplier.lhs_reader.out.valid := reader1.out.valid
      reader1.out.ready := multiplier.lhs_reader.out.ready

      reader1.byteCount := multiplier.lhs_reader.byteCount
      reader1.baseAddr := multiplier.lhs_reader.baseAddr
      reader1.start := multiplier.lhs_reader.start
      multiplier.lhs_reader.finished := reader1.finished
      //reader1 <> multiplier.lhs_reader

      multiplier.rhs_reader.out.bits := reader2.out.bits
      multiplier.rhs_reader.out.valid := reader2.out.valid
      reader2.out.ready := multiplier.rhs_reader.out.ready

      reader2.byteCount := multiplier.rhs_reader.byteCount
      reader2.baseAddr := multiplier.rhs_reader.baseAddr
      reader2.start := multiplier.rhs_reader.start
      multiplier.rhs_reader.finished := reader2.finished
      //reader2 <> multiplier.rhs_reader

      writer.in.bits := multiplier.writer.in.bits
      writer.in.valid := multiplier.writer.in.valid
      multiplier.writer.in.ready := writer.in.ready

      writer.byteCount := multiplier.writer.byteCount
      writer.baseAddr := multiplier.writer.baseAddr
      writer.start := multiplier.writer.start
      multiplier.writer.finished := writer.finished
      multiplier.writer.active := writer.active
      //writer <> multiplier.writerIF

      when(multiplier.done){
        state := s_finished
      }
    }

    is (s_finished) {
      io.finishedWithSlidingWindow := Bool(true)
      io.finished := Bool(true)
      when(~io.start){
        state := s_idle
      }
    }
  }

}
 
