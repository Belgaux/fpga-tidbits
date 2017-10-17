package fpgatidbits.Testbenches

import Chisel._

import fpgatidbits.dma._
import fpgatidbits.streams._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.rosetta._

/*
 * Class that takes in address for a binary matrix and a binary vector and their respective size (Matrix is of numRows X numCols, vector is of numCols X 1. 
 * We iterate over segments of size wordSize in bits. 
 * We iterate over one vector segment and numSimultaneousVectors row segments at a time, that is, we go through the input vector about numRows/numSimultaneousVectors times to get the final result.
 */

class TestBMVM(p: PlatformWrapperParams, _wordSize:Int, _numSimultaneousVectors:Int) extends GenericAccelerator(p) {
  val wordSize = _wordSize
  val numSimultaneousVectors = _numSimultaneousVectors

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

  val writer = Module(new StreamWriter(new StreamWriterParams(
    streamWidth = p.memDataBits, mem = p.toMemReqParams(), chanID = 0
  ))).io

  // Iteration values for row and column segments
  val row_count = Reg(init=UInt(0, width=32))
  val col_count = Reg(init=UInt(0, width=32))

  // The loaded row and column segments
  val row_segments = Vec(numSimultaneousVectors, UInt(width=wordSize))
  val col_segment = Reg(init=UInt(width=wordSize))

  // Result segment
  val res_segment = Vec(numSimultaneousVectors, UInt(width=64))

  // States
  val s_idle :: s_load_column_segment :: s_load_row_segment :: s_compute_res_segment :: s_store_res_segment :: s_finished :: Nil = Enum(UInt(), 6)

  val vector_multipliers = Module(new BinaryVecVec(wordSize)).io

  val state = Reg(init=UInt(s_idle))

  // Default values
  io.finished := Bool(false)
  io.error := reader.error
  io.debug := col_segment

  reader.start :=  Bool(false)
  reader.baseAddr := io.addrV
  reader.byteCount := UInt(wordSize/8)
  reader.req <> io.memPort(0).memRdReq
  reader.rsp <> io.memPort(0).memRdRsp
  plugMemReadPort(0)

  reader.out.ready := Bool(false);

  switch(state){
    is(s_idle){
      when(io.start){
        state := s_load_column_segment
        col_segment := reader.out.bits
        reader.out.ready := Bool(true)
      }
    }

    is(s_load_column_segment){
      when(reader.finished){
        reader.start := Bool(false)
        reader.baseAddr := io.addrM
        state := s_load_row_segment
      }.otherwise{
        reader.start := Bool(true)

        col_segment := reader.out.bits
        reader.out.ready := Bool(true)
      }
    }

    is(s_load_row_segment){
      when(reader.finished){
        reader.start := Bool(false)
        row_count := row_count - UInt(numSimultaneousVectors)
        state := s_compute_res_segment
      }.otherwise{
        reader.baseAddr := io.addrM
        reader.start := Bool(true)
      }
    }

    is(s_compute_res_segment){

    }

    is(s_store_res_segment){

    }

    is(s_finished){

    }
  }
}
