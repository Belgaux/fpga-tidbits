package fpgatidbits.rosetta
import Chisel._

class BinaryGEMM(word_size:Int) extends Module {
  val io = new Bundle {
    val start = Bool(INPUT)
    val done = Bool(OUTPUT)

    val W = Decoupled(UInt(width = word_size)).flip
    val A = Decoupled(UInt(width = word_size)).flip
    val W_R = UInt(INPUT, width = 32)
    val W_C = UInt(INPUT, width = 32)
    val A_R = UInt(INPUT, width = 32)
    val A_C = UInt(INPUT, width = 32)

    val cur_w = UInt(INPUT, width = 32)
    val cur_a = UInt(INPUT, width = 32)
    val max_a = UInt(INPUT, width = 32)
    val max_w = UInt(INPUT, width = 32)

    val write_row = UInt(OUTPUT, width = 32)
    val write_col = UInt(OUTPUT, width = 32)

    val out = Decoupled(SInt(width = word_size))
  }

  val negw = Bool()
  val nega = Bool()
  // Assume little endian ordering
  negw := io.cur_w === io.max_w - UInt(1)
  nega := io.cur_a === io.max_a - UInt(1)

  val row_count = Reg(init = UInt(0, width = 32))
  val col_count = Reg(init = UInt(0, width = 32))
  val elems     = Reg(init = UInt(0, width = 32))
  val acc       = Reg(init = SInt(0, width = word_size))

  val write_row = Reg(init = UInt(0, width = 32))
  val write_col = Reg(init = UInt(0, width = 32))

  val dot = Module(new DotProduct(word_size)).io
  dot.start := Bool(false)
  dot.din0 <> io.W
  dot.din1 <> io.A
  dot.dout.ready := Bool(false)

  io.done := Bool(false)
  io.out.bits  := acc
  io.out.valid := Bool(false)
  io.write_row := write_row
  io.write_col := write_col

  val s_idle :: s_row :: s_col :: s_vec :: s_write :: s_done :: Nil = Enum(UInt(), 6)
  val state = Reg(init = UInt(s_idle))

  switch (state) {
    is (s_idle) {
      row_count := UInt(0)
      col_count := UInt(0)
      write_row := UInt(0)
      write_col := UInt(0)
      elems := UInt(0)
      acc := SInt(0)
      when (io.start) { state := s_vec }
    }

    is (s_row) {
      when (row_count === io.W_R) {
        state := s_done
      }
      .elsewhen (row_count != io.W_R
          && col_count === io.A_C) { // When we have finished all columns for this row
        write_row := row_count
        row_count := row_count + UInt(1)
        col_count := UInt(0)
        state := s_write
      }
      .elsewhen (row_count != io.W_R
          && col_count != io.A_C) { // When we have more columns to do for this row
        acc := SInt(0)
        state := s_vec
      }
    }

    is (s_col) {
      when (elems === io.W_C) { // Done with one vector-vector product
        write_col := col_count // save the output column 
        col_count := col_count + UInt(1)
        elems := UInt(0)
        state := s_row
      }
      .elsewhen (elems != io.W_C) { // vector-vector product in progress
        state := s_vec
      }
    }

    // this state is one chunk of a vector-vector multiplication
    is (s_vec) {
      dot.dout.ready := Bool(true)
      when (dot.done) {
        printf("dot.dout.bits=%b\n", dot.dout.bits)
        acc := Mux((UInt(negw) ^ UInt(nega)) === UInt(1),
          acc - (dot.dout.bits << (io.cur_w + io.cur_a)),
          acc + (dot.dout.bits << (io.cur_w + io.cur_a)))
        elems := elems + UInt(word_size)
        state := s_col
      }
      .otherwise {
        dot.start := Bool(true)
      }
    }

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
