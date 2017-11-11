package fpgatidbits.Testbenches

import Chisel._

import fpgatidbits.dma._
import fpgatidbits.ocm._
import fpgatidbits.streams._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.rosetta._


// Ties together the sliding window module and the matrix multiplication module
// Presumes input image is of form channels/bitplanes/rows/columns with each row padded to 64 bits
class TestConvolution(p: PlatformWrapperParams, _wordSizeInBits:Int) extends GenericAccelerator(p) {
  val wordSizeInBits = _wordSizeInBits
  val wordSizeInBytes = wordSizeInBits/8

  val wordBitExponent = log2Up(wordSizeInBits)
  val wordByteExponent = wordBitExponent - 3

  val numMemPorts = 3

  val io = new GenericAcceleratorIF(numMemPorts, p){
    val imageAddr = UInt(INPUT, width=64)
    val filterAddr = UInt(INPUT, width=64)
    val outputAddr = UInt(INPUT, width=64)
    val tempAddr = UInt(INPUT, width=64)

    val imageWidth = UInt(INPUT, width=32)
    val imageHeight = UInt(INPUT, width=32)
    val imageNumBits = UInt(INPUT, width=4)
    val imageNumChannels = UInt(INPUT, width=16)

    val stride = UInt(INPUT, width=4)
    val windowSize = UInt(INPUT, width=4)
    val numOutputChannels = UInt(INPUT, width=16)

    val filtersNumBits = UInt(INPUT, width=4)

    val start = Bool(INPUT)
    val finishedWithSlidingWindow = Bool(OUTPUT)
    val finished = Bool(OUTPUT)
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
    reader.baseAddr := UInt(0)
    reader.byteCount := UInt(0)
    reader.out.ready := Bool(false)
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

    wr.baseAddr := UInt(0)
    wr.byteCount := UInt(0)
    wr.in.valid := Bool(false)
    wr.in.bits := UInt(0)
    wr
  }


  val reader1 = initialize_reader(0)
  val reader2 = initialize_reader(1)
  val writer = initialize_writer(2)


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

  val resultChannelSize = Reg(init=UInt(0, width=32))
  val resultTotalSize = Reg(init=UInt(0, width=32))

  windowSizeSquared := io.windowSize * io.windowSize
  slidedWindowNumWidth := (io.imageWidth - io.windowSize)/io.stride + UInt(1)
  slidedWindowNumHeight := (io.imageHeight - io.windowSize)/io.stride + UInt(1)
  slidedWindowNumRowsInBitplane := slidedWindowNumHeight * slidedWindowNumWidth
  slidedWindowSizeInWords := ((windowSizeSquared + UInt(wordSizeInBits - 1)) >> wordBitExponent)
  slidedWindowRowSizeInWords := slidedWindowSizeInWords * io.imageNumChannels
  slidedWindowBitplaneSizeInWords := slidedWindowRowSizeInWords * slidedWindowNumRowsInBitplane
  slidedWindowTotalSizeInBytes := (io.imageNumBits * slidedWindowBitplaneSizeInWords) << wordByteExponent

  resultChannelSize := slidedWindowNumRowsInBitplane << wordByteExponent // Assume one word per output element
  resultTotalSize := resultChannelSize * io.numOutputChannels

  val s_idle :: s_running_sliding_window :: s_running_multiplication :: s_finished :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_idle)

  io.finished := Bool(false)

  val windowSlider = Module(new ModuleSlidingWindowBitplanes(p, wordSizeInBits)).io

  windowSlider.numCols := io.imageWidth
  windowSlider.numRows := io.imageHeight
  windowSlider.numChannels := io.imageNumChannels
  windowSlider.numBits := io.imageNumBits
  windowSlider.stride := io.stride
  windowSlider.windowSize := io.windowSize
  windowSlider.addrImage := io.imageAddr
  windowSlider.addrResult := io.tempAddr
  windowSlider.start := Bool(false)


  val multiplier = Module(new ModuleBinaryGEMM(p, wordSizeInBits)).io
  multiplier.lhs_addr := io.filterAddr
  multiplier.rhs_addr := io.tempAddr
  multiplier.res_addr := io.outputAddr
  multiplier.res_byte_count := resultTotalSize

  multiplier.lhs_rows := io.numOutputChannels 
  multiplier.lhs_cols := slidedWindowRowSizeInWords << wordBitExponent
  multiplier.lhs_bits := io.filtersNumBits
  multiplier.lhs_issigned := Bool(true) // Is this ok?

  // Double check that rows is rows in memory, and not of the transposed of what's in memory
  multiplier.rhs_rows := slidedWindowNumRowsInBitplane
  multiplier.rhs_cols := slidedWindowSizeInWords << wordBitExponent
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

  multiplier.readerIF1.finished := Bool(false)
  multiplier.readerIF1.out.bits := UInt(0)
  multiplier.readerIF1.out.valid := Bool(false)

  multiplier.readerIF2.finished := Bool(false)
  multiplier.readerIF2.out.bits := UInt(0)
  multiplier.readerIF2.out.valid := Bool(false)

  multiplier.writerIF.finished := Bool(false)
  multiplier.writerIF.in.ready := Bool(false)
  multiplier.writerIF.active := Bool(false)


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
        printf("Going to multiplication\n") 
        state := s_running_multiplication
      }
    }

    is (s_running_multiplication) { // TODO: this section is (probably) the cause of a crash. Checkcheck
      printf("In running multiplication\n");
      multiplier.start := Bool(true)

      multiplier.readerIF1.out.bits := reader1.out.bits
      multiplier.readerIF1.out.valid := reader1.out.valid
      reader1.out.ready := multiplier.readerIF1.out.ready

      reader1.byteCount := multiplier.readerIF1.byteCount
      reader1.baseAddr := multiplier.readerIF1.baseAddr
      reader1.start := multiplier.readerIF1.start
      multiplier.readerIF1.finished := reader1.finished
      //reader1 <> multiplier.readerIF1

      multiplier.readerIF2.out.bits := reader2.out.bits
      multiplier.readerIF2.out.valid := reader2.out.valid
      reader2.out.ready := multiplier.readerIF2.out.ready

      reader2.byteCount := multiplier.readerIF2.byteCount
      reader2.baseAddr := multiplier.readerIF2.baseAddr
      reader2.start := multiplier.readerIF2.start
      multiplier.readerIF2.finished := reader2.finished
      //reader2 <> multiplier.readerIF2

      writer.in.bits := multiplier.writerIF.in.bits
      writer.in.valid := multiplier.writerIF.in.valid
      multiplier.writerIF.in.ready := writer.in.ready

      writer.byteCount := multiplier.writerIF.byteCount
      writer.baseAddr := multiplier.writerIF.baseAddr
      writer.start := multiplier.writerIF.start
      multiplier.writerIF.finished := writer.finished
      multiplier.writerIF.active := writer.active
      //writer <> multiplier.writerIF

      when(multiplier.done){
        state := s_finished
      }
    }

    is (s_finished) {
      io.finished := Bool(true)
      when(~io.start){
        state := s_idle
      }
    }
  }

}
 
