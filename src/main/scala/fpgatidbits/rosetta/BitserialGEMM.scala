package fpgatidbits.rosetta

import Chisel._


// Bitserial General Matrix Multiplication
// A must be given transposed

class BitserialGEMM(word_size: Int, W_depth:Int, A_depth:Int) extends Module {

  val io = new Bundle {
    val start = Bool(INPUT)
    val W = Decoupled(SInt(width = word_size)).flip
    val A = Decoupled(SInt(width = word_size)).flip
    val W_start = Bool(OUTPUT)
    val A_start = Bool(OUTPUT)
    val W_R = UInt(INPUT, width = 32)
    val W_C = UInt(INPUT, width = 32)
    val A_R = UInt(INPUT, width = 32)
    val A_C = UInt(INPUT, width = 32)
    val out = Decoupled(SInt(width = word_size))
    val done = Bool(OUTPUT)
  }

  val acc = Reg(init = SInt(0, width = word_size))
  val row_count = Reg(init = UInt(0, width = 32))
  val col_count = Reg(init = UInt(0, width = 32))

  // Track how many elems we've computed from the input stream for one VV pair
  val elems = Reg(init = UInt(0, width = 32))

  val Manager = Module(new BitserialManager(word_size, 2, 2)).io
  Manager.start := Bool(false)
  Manager.W <> io.W
  Manager.A <> io.A

  io.done := Bool(false)
  io.out.bits := acc
  io.out.valid := Bool(false)
  io.W_start := Bool(false)
  io.A_start := Bool(false)

  val s_idle :: s_row :: s_col :: s_vec :: s_write :: s_done :: Nil = Enum(UInt(), 6)
  val state = Reg(init = UInt(s_idle))

  switch (state) {
    is (s_idle) {
      row_count := UInt(0)
      col_count := UInt(0)
      elems := UInt(0)
      acc := UInt(0)
      when (io.start) { state := s_vec }
    }

    is (s_row) {
      acc := UInt(0)
      when (row_count === io.W_R) {
        state := s_done
      }
      .elsewhen (row_count != io.W_R
                && col_count === io.A_C) {
        row_count := row_count + UInt(1)
        col_count := UInt(0)
      }
      .elsewhen (row_count != io.W_R
                && col_count != io.A_C) {
        state := s_vec
      }
    }

    is (s_col) {
      when (elems === io.W_C) {
        col_count := col_count + UInt(1)
        elems := UInt(0)
        state := s_write
      }
      .elsewhen (elems != io.W_C) {
        state := s_vec
      }
    }

    // This state represents one whole vector-vector MUL
    is (s_vec) {
      when (Manager.done) {
        acc := acc + Manager.out
        elems := elems + UInt(word_size)
        state := s_col
      }
      .otherwise {
        Manager.start := Bool(true)
        io.W_start := Bool(true)
        io.A_start := Bool(true)
      }
    }

    // TODO(Oscar): maybe change this to buffering output in BRAM/cache?
    is (s_write) {
      io.out.valid := Bool(true)
      when (io.out.ready) {
        state := s_row
      }
    }

    is (s_done) {
      io.done := Bool(true)
      when (!io.start) { state := s_idle }
    }
  }
}
