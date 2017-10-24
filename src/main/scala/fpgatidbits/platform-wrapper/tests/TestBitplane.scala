package fpgatidbits.Testbenches

import Chisel._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.streams._
import fpgatidbits.dma._

class TestBitplane(p: PlatformWrapperParams, word_size:Int, num_active_vecs:Int) extends GenericAccelerator(p) {

  val numMemPorts = 2
  val io = new GenericAcceleratorIF(numMemPorts, p) {
    val start = Bool(INPUT)
    val addrM = UInt(INPUT, width=64)
    val addrV = UInt(INPUT, width=64)
    val addrR = UInt(INPUT, width=64)
    val numRows = UInt(INPUT, width=32)
    val numCols = UInt(INPUT, width=32)
    val m_depth = UInt(INPUT, width=32)
    val v_depth = UInt(INPUT, width=32)
    val stride = UInt(INPUT, width=32) // Bytes per row
    val finished = Bool(OUTPUT)
  }

  // Iteration values for row and column segments
  val row_count = Reg(init=UInt(0, width=32))
  val col_count = Reg(init=UInt(0, width=32))
  val global_row_count = Reg(init=UInt(0, width=32))
  val bytes_per_word = UInt(word_size/8)


  // Bitserial utils
  val cur_m_depth = Reg(init=UInt(0, width=32))
  val cur_v_depth = Reg(init=UInt(0, width=32))
  val sign_m = SInt()
  val sign_v = SInt()
  val sign_bit = SInt()
  val significance = SInt()
  sign_m       := Mux(cur_m_depth === io.m_depth - UInt(1), SInt(-1), SInt(1))
  sign_v       := Mux(cur_v_depth === io.v_depth - UInt(1), SInt(-1), SInt(1))
  sign_bit     := sign_m * sign_v
  significance := SInt(1) << (cur_m_depth + cur_v_depth)


  val sr = Module(new StreamReader(new StreamReaderParams(
    streamWidth = word_size, fifoElems = 8, mem = p.toMemReqParams(),
    maxBeats = 1, chanID = 0, disableThrottle = true
  ))).io

  val wr = Module(new StreamWriter(new StreamWriterParams(
    streamWidth = p.memDataBits, mem = p.toMemReqParams(), chanID = 0
  ))).io

  wr.byteCount := UInt(num_active_vecs) * bytes_per_word
  wr.baseAddr := io.addrR + global_row_count * bytes_per_word
  wr.start := Bool(false)
  wr.req <> io.memPort(1).memWrReq
  wr.wdat <> io.memPort(1).memWrDat
  io.memPort(1).memWrRsp <> wr.rsp
  plugMemReadPort(1)

  val row_segment = Mem(SInt(width=word_size), num_active_vecs)
  val col_segment = Reg(init=SInt(0, width=word_size))
  val res_segment = Mem(SInt(width=word_size), num_active_vecs)
  val index = Reg(init=UInt(0, width=32))

  val s_idle :: s_debug_transition :: s_load_column_segment :: s_load_row_segment :: s_compute_res_segment :: s_store_res_segment :: s_finished :: s_done_check :: s_col_bits :: Nil = Enum(UInt(), 9)
  val state = Reg(init=UInt(s_idle))

  io.finished := Bool(false)
  sr.out.ready := Bool(false)

  sr.start := io.start
  sr.baseAddr := io.addrV + col_count
  sr.byteCount := bytes_per_word
  sr.req <> io.memPort(0).memRdReq
  io.memPort(0).memRdRsp <> sr.rsp
  plugMemWritePort(0)

  // Combine result registers into a single stream that we can pass to writer 
  val intr = Module(new StreamInterleaver(num_active_vecs, UInt())).io
  for (i <- 0 until num_active_vecs) {
    intr.in(i).bits := res_segment(i)
    intr.in(i).valid := Bool(false)
  }
  intr.out <> wr.in

  val row_done = Bool()
  row_done := col_count === io.stride

  switch(state) {
    is(s_idle){
      sr.start := Bool(false)
      global_row_count := UInt(0)
      row_count := UInt(0)
      col_count := UInt(0)
      when (io.start) { 
        state := s_load_column_segment
      }
    }

    is(s_debug_transition) {
      col_count := Mux(row_done, UInt(0), col_count)
      global_row_count := Mux(row_done, global_row_count + UInt(num_active_vecs),
        global_row_count)
      row_count := Mux(row_done, row_count, global_row_count)

      sr.start := Bool(false)
      state := s_load_column_segment
    
    }

    is(s_load_column_segment){
      sr.out.ready := Bool(true)
      sr.baseAddr := io.addrV + col_count
      when (sr.out.valid) {
        col_segment := sr.out.bits
        state := s_load_row_segment
        sr.start := Bool(false)
      }
    }
    is(s_load_row_segment) {
      when(index != UInt(num_active_vecs)) {
        sr.out.ready := Bool(true)
        sr.baseAddr := io.addrM + io.stride * row_count + col_count
        when(sr.out.valid) {
          row_segment(index) := sr.out.bits
          index := index + UInt(1)
          row_count := row_count + UInt(1)
          sr.start := Bool(false)
        }
      }
      .otherwise {
        sr.start := Bool(false)
        index := UInt(0)
        col_count := col_count + bytes_per_word
        state := s_compute_res_segment
      }
    }

    is(s_compute_res_segment){
      sr.start := Bool(false)

      cur_m_depth := UInt(0)
      cur_v_depth := UInt(0)

      state := s_col_bits
    }

    // BITSERIAL
    is(s_done_check) {
      sr.start := Bool(false)
      
      when (cur_m_depth === io.m_depth
            && cur_v_depth === io.v_depth) {
        state := s_store_res_segment
      }
      .otherwise {
        cur_v_depth := UInt(0)
        state := s_col_bits
      }
    }

    is(s_col_bits) {
      sr.start := Bool(false)

      when (cur_m_depth != io.m_depth
            && cur_v_depth === io.v_depth) {
        cur_m_depth := cur_m_depth + UInt(1)
        state := s_done_check
      }
      .elsewhen (cur_m_depth != io.m_depth
            && cur_v_depth != io.v_depth) {
        for (i <- 0 until num_active_vecs) {
          res_segment(i) := res_segment(i) + (PopCount((col_segment(cur_v_depth) & row_segment(i)(cur_m_depth))) * sign_bit * significance)
        }
        cur_v_depth := cur_v_depth + UInt(1)
      }
    }

    is(s_store_res_segment) {
      sr.start := Bool(false)
      wr.start := Bool(true)
      when (index === UInt(num_active_vecs)) {
        when (wr.finished) {
          index := UInt(0)

          state := Mux(col_count === io.stride 
            && global_row_count === io.numRows - UInt(num_active_vecs),
            s_finished, s_debug_transition)
          
          for (i <- 0 until num_active_vecs) {
            res_segment(i) := Mux(col_count === io.stride,
              SInt(0), res_segment(i)) 
          } 
        }
      }
      .otherwise {
        intr.in(index).valid := Bool(true)
        when (wr.in.ready) {
          index := index + UInt(1)
        }
      }
    }

    is(s_finished){
      io.finished := Bool(true)
      sr.start := Bool(false)
      when (!io.start) {
        state := s_idle 
      }
    }
  }
}
