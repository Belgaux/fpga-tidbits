package fpgatidbits.Testbenches

import Chisel._

import fpgatidbits.dma._
import fpgatidbits.streams._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.rosetta._

/*
 * Class that takes in address for a binary matrix and a binary vector and their respective size (Matrix is of numRows X numCols, vector is of numCols X 1. 
 * We iterate over segments of size wordSize in bits. 
 * We iterate over one vector segment and numActiveVecs row segments at a time, that is, we go through the input vector about numRows/numActiveVecs times to get the final result.
 */

class TestBMVM(p: PlatformWrapperParams, _wordSize:Int, _numActiveVecs:Int) extends GenericAccelerator(p) {
  val wordSize = _wordSize
  val numActiveVecs = _numActiveVecs

  val numMemPorts = 2
  val io = new GenericAcceleratorIF(numMemPorts, p){
    val start = Bool(INPUT)
    val addrM = UInt(INPUT, width=64)
    val addrV = UInt(INPUT, width=64)
    val addrR = UInt(INPUT, width=64)
    val numRows = UInt(INPUT, width=32)
    val numCols = UInt(INPUT, width=32)
    val stride = UInt(INPUT, width=32) // Number of bits between start of consecutive rows. Should be a multiple of wordSize

    val debug = UInt(OUTPUT, width=32)
    val error = Bool(OUTPUT)
    val finished = Bool(OUTPUT)
  }

  val reader = Module(new StreamReader(new StreamReaderParams(
    streamWidth = wordSize, fifoElems = 8, mem = p.toMemReqParams(),
    maxBeats = 1, chanID = 0, disableThrottle = true
  ))).io

  val wr = Module(new StreamWriter(new StreamWriterParams(
    streamWidth = p.memDataBits, mem = p.toMemReqParams(), chanID = 0
  ))).io
  wr.byteCount := UInt(numActiveVecs * p.memDataBits/8) 
  wr.baseAddr := io.addrR
  //wr.baseAddr := wr.baseAddr + wr.byteCount
  wr.start := io.start
  wr.req <> io.memPort(1).memWrReq
  wr.wdat <> io.memPort(1).memWrDat
  io.memPort(1).memWrRsp <> wr.rsp
  plugMemReadPort(1)

  // Iteration values for row and column segments
  val row_count = Reg(init=UInt(0, width=32))
  val col_count = Reg(init=UInt(0, width=32))

  val row_segment = Vec.fill(numActiveVecs) {Reg(init=UInt(0, wordSize))}
  val col_segment = Reg(init=UInt(0, width=wordSize))
  val index = Reg(init=UInt(0, log2Up(numActiveVecs)+1))
  val res_segment = Vec.fill(numActiveVecs) {Reg(init=UInt(0, 64))}

  val s_idle :: s_load_column_segment :: s_load_row_segment :: s_compute_res_segment :: s_store_res_segment :: s_finished :: Nil = Enum(UInt(), 6)
  val state = Reg(init=UInt(s_idle))

  // Default values
  io.finished := Bool(false)
  io.error := reader.error
  io.debug := col_segment
  reader.out.ready := Bool(false)

  reader.start := io.start
  reader.baseAddr := io.addrV
  reader.byteCount := UInt(wordSize/8)
  reader.req <> io.memPort(0).memRdReq
  io.memPort(0).memRdRsp <> reader.rsp
  plugMemWritePort(0)

  val intr = Module(new StreamInterleaver(numActiveVecs, UInt())).io
  for (i <- 0 until numActiveVecs) {
    intr.in(i).bits := res_segment(i)  
    intr.in(i).valid := Bool(false)
  }

  val sg = Module(new SequenceGenerator(p.memDataBits)).io
  sg.start := io.start
  sg.init := UInt(1)
  sg.step := UInt(0)
  sg.count := UInt(numActiveVecs)

  StreamRepeatElem(intr.out, sg.seq) <> wr.in

  switch(state){
    is(s_idle){
      row_count := UInt(0)
      col_count := UInt(0)
      when (io.start) { 
        state := s_load_column_segment
      }
    }

    is(s_load_column_segment){
      reader.out.ready := Bool(true)
      when (reader.out.valid) {
        col_segment := reader.out.bits
        state := s_load_row_segment
        reader.start := Bool(false)
      }
    }
    is(s_load_row_segment) {
      reader.baseAddr := io.addrM + io.stride * row_count + col_count
      reader.out.ready := Bool(true)
      when (reader.out.valid) {
        when (index != UInt(numActiveVecs)) {
          row_segment(index) := reader.out.bits
          row_count := row_count + UInt(1)
          index := index + UInt(1)
          reader.start := Bool(false)
        }
        .otherwise {
          for (i <- 0 until numActiveVecs) {
            printf("%b\n", row_segment(i))
          }
          printf("col_count=%d\n", col_count)
          col_count := col_count + UInt(wordSize/8)
          index := UInt(0)

          for (i <- 0 until numActiveVecs) {
            res_segment(i) := res_segment(i) + PopCount(col_segment & row_segment(i))
          }
          state := s_store_res_segment
        }
      }
    }

    is(s_compute_res_segment){

    }

    is(s_store_res_segment) {
      when (index != UInt(numActiveVecs)) {
        intr.in(index).valid := Bool(true)
        index := index + UInt(1)
      }
      .otherwise {
        when (wr.finished) { state := s_finished }
      }
    }

    is(s_finished){
      io.finished := Bool(true)
      when (!io.start) {
        for (i<-0 until numActiveVecs) {
          printf("%d\n", res_segment(i))
        }
        state := s_idle 
      }
    }
  }
}
