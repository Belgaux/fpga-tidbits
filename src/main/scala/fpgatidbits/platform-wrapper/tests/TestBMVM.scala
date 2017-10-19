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
    val stride = UInt(INPUT, width=32) // Bytes per row

    val debug = UInt(OUTPUT, width=32)
    val error = Bool(OUTPUT)
    val finished = Bool(OUTPUT)
  }

  // Iteration values for row and column segments
  val row_count = Reg(init=UInt(0, width=32))
  val col_count = Reg(init=UInt(0, width=32))
  val global_row_count = Reg(init=UInt(0, width=32))
  val col_chunk = UInt(wordSize/8)

  val reader = Module(new StreamReader(new StreamReaderParams(
    streamWidth = wordSize, fifoElems = 8, mem = p.toMemReqParams(),
    maxBeats = 1, chanID = 0, disableThrottle = true
  ))).io

  val wr = Module(new StreamWriter(new StreamWriterParams(
    streamWidth = p.memDataBits, mem = p.toMemReqParams(), chanID = 0
  ))).io
  wr.byteCount := UInt(numActiveVecs * wordSize/8) 
  wr.baseAddr := io.addrR + global_row_count * UInt(wordSize/8)
  wr.start := Bool(false)
  wr.req <> io.memPort(1).memWrReq
  wr.wdat <> io.memPort(1).memWrDat
  io.memPort(1).memWrRsp <> wr.rsp
  plugMemReadPort(1)

  val row_segment = Vec.fill(numActiveVecs) {Reg(init=UInt(0, wordSize))}
  val col_segment = Reg(init=UInt(0, width=wordSize))
  val index = Reg(init=UInt(0, width=32))
  val res_segment = Vec.fill(numActiveVecs) {Reg(init=UInt(0, wordSize))}

  val s_idle :: s_debug_transition :: s_load_column_segment :: s_load_row_segment :: s_compute_res_segment :: s_store_res_segment :: s_finished :: Nil = Enum(UInt(), 7)
  val state = Reg(init=UInt(s_idle))

  // Default values
  io.finished := Bool(false)
  io.error := reader.error
  io.debug := col_segment
  reader.out.ready := Bool(false)

  reader.start := io.start
  reader.baseAddr := io.addrV + col_count
  reader.byteCount := col_chunk
  reader.req <> io.memPort(0).memRdReq
  io.memPort(0).memRdRsp <> reader.rsp
  plugMemWritePort(0)

  val intr = Module(new StreamInterleaver(numActiveVecs, UInt())).io
  for (i <- 0 until numActiveVecs) {
    intr.in(i).bits := res_segment(i)  
    intr.in(i).valid := Bool(false)
  }
  intr.out <> wr.in

  val row_done = Bool()
  row_done := col_count === io.stride

  
  switch(state) {
    is(s_idle){
      global_row_count := UInt(0)
      row_count := UInt(0)
      col_count := UInt(0)
      when (io.start) { 
        state := s_load_column_segment
      }
    }

    is(s_debug_transition) {
      
      col_count := Mux(row_done, UInt(0), col_count)
      global_row_count := Mux(row_done, global_row_count + UInt(numActiveVecs),
        global_row_count)
      row_count := Mux(row_done, row_count, global_row_count)

      // flushing reader
      reader.start := Bool(false)
      reader.out.ready := Bool(true)
      state := s_load_column_segment
    
    }

    is(s_load_column_segment){
      reader.out.ready := Bool(true)
      reader.baseAddr := io.addrV + col_count
      when (reader.out.valid) {
        col_segment := reader.out.bits
        state := s_load_row_segment
        reader.start := Bool(false)
      }
    }
    is(s_load_row_segment) {
      when(index != UInt(numActiveVecs)) {
        reader.out.ready := Bool(true)
        reader.baseAddr := io.addrM + io.stride * row_count + col_count
        when(reader.out.valid) {
          row_segment(index) := reader.out.bits
          index := index + UInt(1)
          row_count := row_count + UInt(1)
          reader.start := Bool(false)
        }
      }.otherwise { // Done with one row chunk
        index := UInt(0)

        col_count := col_count + col_chunk
        
        state := s_compute_res_segment
      }
    }

    is(s_compute_res_segment){
/*
      printf("%b\n", col_segment)
      printf("col_count=%d\n", col_count)
      printf("row_count=%d\n", row_count)
      printf("global_row_count=%d\n", global_row_count)
      for (i <- 0 until numActiveVecs) {
        printf("%b\n", row_segment(i))
      }
*/
      for (i <- 0 until numActiveVecs) {
        res_segment(i) := res_segment(i) + PopCount(col_segment & row_segment(i))
      }
      state := s_store_res_segment
    }

    is(s_store_res_segment) {
      wr.start := Bool(true)
      when (index === UInt(numActiveVecs)) {
        when (wr.finished) {
          index := UInt(0)

          state := Mux(col_count === io.stride 
            && global_row_count === io.numRows - UInt(numActiveVecs),
            s_finished, s_debug_transition)
          
          for (i <- 0 until numActiveVecs) {
            res_segment(i) := Mux(col_count === io.stride,
              UInt(0), res_segment(i)) 
          } 
        }
      }
      .otherwise {
        intr.in(index).valid := Bool(true)
        when (wr.in.ready) {
          index := index + UInt(1)
          //printf("intr.out.bits=%d\n", intr.out.bits)
        }
      }
    }

    is(s_finished){
      io.finished := Bool(true)
      when (!io.start) {
        state := s_idle 
      }
    }
  }
}
