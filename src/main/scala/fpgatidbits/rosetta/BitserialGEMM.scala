package fpgatidbits.Testbenches

import Chisel._
import fpgatidbits.dma._
import fpgatidbits.ocm._
import fpgatidbits.rosetta._
import fpgatidbits.PlatformWrapper._

// Bitserial General Matrix Multiplication 
// Right-hand side must be given transposed
// Result is also produced transposed

class BitserialGEMM(word_size : Int, p: PlatformWrapperParams) extends Module {

  val output_queue_size = 8
  val input_queue_size = 8
  val bytes_per_elem = UInt(word_size/8, width=8)

  val io = new Bundle {
    val lhs_addr = UInt(INPUT, width = 64)
    val rhs_addr = UInt(INPUT, width = 64)
    val res_addr = UInt(INPUT, width = 64)
    val num_chn = UInt(INPUT, width = 16)

    val lhs_rows = UInt(INPUT, width = 16)
    val lhs_cols = UInt(INPUT, width = 16)
    val lhs_bits = UInt(INPUT, width = 8)
    val lhs_issigned = Bool(INPUT)

    val rhs_rows = UInt(INPUT, width = 16)
    val rhs_cols = UInt(INPUT, width = 16)
    val rhs_bits = UInt(INPUT, width = 8)
    val rhs_issigned = Bool(INPUT)

    val start = Bool(INPUT)
    val done = Bool(OUTPUT)

    val lhs_reader = new StreamReaderIF(word_size, p.toMemReqParams).flip
    val rhs_reader = new StreamReaderIF(word_size, p.toMemReqParams).flip
    val writer     = new StreamWriterIF(word_size, p.toMemReqParams).flip
  }

  //////// REGISTERS

  val write_index = Reg(init=UInt(0, width=32))
  val num_words = Reg(init=UInt(0, width = 32))

  // For each row in W, for each row in A
  // Equivalent to i,j in Yaman's 'gemmBitserial_generic_naive'
  val row_rhs = Reg(init = UInt(0, width = 16))
  val row_lhs = Reg(init = UInt(0, width = 16))

  // Bit-plane counters
  val lbit = Reg(init=UInt(0, width = 16))
  val rbit = Reg(init=UInt(0, width = 16))

  val chn = Reg(init=UInt(0, width = 16))

  // Accumulate result of all bitplanes for each row-pair
  val row_res = Reg(init=SInt(0, width=word_size))
  // Acuumulate AND-PCNT result within each bitplane row-pair
  val acc_dot_row = Reg(init=UInt(0, width=word_size))

  // Parameters for stream readers and pipeline registers
  val lhs_bytes_per_bitplane     = Reg(init=UInt(0))
  val rhs_bytes_per_bitplane     = Reg(init=UInt(0))

  val lhs_bytes_per_bitplane_tmp = Reg(init=UInt(0))
  val rhs_bytes_per_bitplane_tmp = Reg(init=UInt(0))

  val bytes_per_row              = Reg(init=UInt(0))

  val lhs_base_addr              = Reg(init=UInt(0))
  val rhs_base_addr              = Reg(init=UInt(0))

  val lhs_row_stride             = Reg(init=UInt(0))
  val rhs_row_stride             = Reg(init=UInt(0))

  val lhs_bitplane_stride        = Reg(init=UInt(0))
  val rhs_bitplane_stride        = Reg(init=UInt(0))

  val lhs_bytes_per_channel     = Reg(init=UInt(0))
  val rhs_bytes_per_channel     = Reg(init=UInt(0))

  val lhs_channel_stride        = Reg(init=UInt(0))
  val rhs_channel_stride        = Reg(init=UInt(0))

  val res_byte_count = Reg(init=UInt(0, width=32))
  val start_writer = Reg(init=UInt(0, width=1))


  /////// WIRES

  // Sign-bits for bitserial algorithm
  // Assume that bitplanes are stored in "little endian" order
  val neg_lhs = Bool()
  val neg_rhs = Bool()
  neg_lhs := io.lhs_issigned && (lbit === (io.lhs_bits - UInt(1)))
  neg_rhs := io.rhs_issigned && (rbit === (io.rhs_bits - UInt(1)))
  io.done := Bool(false)

  //////// OUTPUT
 
  io.writer.baseAddr := io.res_addr
  io.writer.byteCount := res_byte_count
  io.writer.start := start_writer

  val out_queue = Decoupled(SInt(width=word_size))
  out_queue.valid := Bool(false)
  out_queue.bits := UInt(666) // debug value, should never happen
  io.writer.in <> FPGAQueue(out_queue, output_queue_size)


  ///////// INPUT

  io.lhs_reader.baseAddr := lhs_base_addr
  io.lhs_reader.byteCount := bytes_per_row
  io.lhs_reader.start := Bool(false)
  io.rhs_reader.baseAddr := rhs_base_addr
  io.rhs_reader.byteCount := bytes_per_row
  io.rhs_reader.start := Bool(false)

  def start_readers() = {
    io.lhs_reader.start := Bool(true)
    io.rhs_reader.start := Bool(true)  
  }


  ///////// PROCESSING ELEMENT

  val dot = Module(new DotProduct(word_size)).io
  dot.din0 <> FPGAQueue(io.lhs_reader.out, input_queue_size)
  dot.din1 <> FPGAQueue(io.rhs_reader.out, input_queue_size)
  val dot_queue = FPGAQueue(dot.dout, input_queue_size)
  dot_queue.ready := Bool(false)


  ///////// STATE MACHINE

  val s_idle :: s_prep_reader_static0 :: s_prep_reader_static1 :: s_prep_reader_static2 :: s_prep_reader_dynamic0 :: s_prep_reader_dynamic1 :: s_prep_reader_dynamic2 :: s_prep_reader_dynamic3 :: s_start_readers :: s_inner :: s_inner_test :: s_inner_post :: s_rbit_test :: s_lbit_test :: s_row_lhs_test :: s_row_rhs_test :: s_chn_test :: s_wait :: s_done :: Nil = Enum(UInt(), 19)
  val state = Reg(init=UInt(s_idle))

  switch (state) {
    is (s_idle) {
      row_rhs       := UInt(0)
      row_lhs       := UInt(0)
      row_res       := SInt(0)
      lbit          := UInt(0)
      rbit          := UInt(0)
      write_index   := UInt(0)
      acc_dot_row   := UInt(0)
      num_words     := UInt(0)

      when (io.start) {
        assert(io.lhs_cols === io.rhs_cols)

        state := s_prep_reader_static0
      }
    }

    is (s_prep_reader_static0) {
      bytes_per_row              := bytes_per_elem * io.lhs_cols
      lhs_bytes_per_bitplane_tmp := bytes_per_elem * io.lhs_rows
      rhs_bytes_per_bitplane_tmp := bytes_per_elem * io.rhs_rows
      res_byte_count             := bytes_per_elem * io.lhs_rows
      state := s_prep_reader_static1
    }

    is (s_prep_reader_static1) {
      lhs_bytes_per_bitplane := lhs_bytes_per_bitplane_tmp * io.lhs_cols
      rhs_bytes_per_bitplane := rhs_bytes_per_bitplane_tmp * io.rhs_cols
      res_byte_count         := res_byte_count * io.rhs_rows
      state := s_prep_reader_static2
    }
 
    is (s_prep_reader_static2) {
      lhs_bytes_per_channel := lhs_bytes_per_bitplane * io.lhs_bits
      rhs_bytes_per_channel := rhs_bytes_per_bitplane * io.rhs_bits
      res_byte_count        := res_byte_count * io.num_chn
      state := s_prep_reader_dynamic0
    }

    is (s_prep_reader_dynamic0) {
      lhs_row_stride      := row_lhs * bytes_per_row 
      rhs_row_stride      := row_rhs * bytes_per_row 
      lhs_bitplane_stride := lbit * lhs_bytes_per_bitplane
      rhs_bitplane_stride := rbit * rhs_bytes_per_bitplane
      lhs_channel_stride  := chn * lhs_bytes_per_channel
      rhs_channel_stride  := chn * rhs_bytes_per_channel
      state := s_prep_reader_dynamic1
    }
   
    is (s_prep_reader_dynamic1) {
      lhs_base_addr := io.lhs_addr + lhs_row_stride
      rhs_base_addr := io.rhs_addr + rhs_row_stride
      state := s_prep_reader_dynamic2
    } 

    is (s_prep_reader_dynamic2) {
      lhs_base_addr := lhs_base_addr + lhs_bitplane_stride
      rhs_base_addr := rhs_base_addr + rhs_bitplane_stride
      state := s_prep_reader_dynamic3
    }

    is (s_prep_reader_dynamic3) {
      lhs_base_addr := lhs_base_addr + lhs_channel_stride
      rhs_base_addr := rhs_base_addr + rhs_channel_stride
      state := s_start_readers
    }

    is (s_start_readers) {
      start_readers()
      start_writer := UInt(1)
      state := s_inner
    }

    is (s_inner) {
      when (dot_queue.valid) {
        dot_queue.ready := Bool(true)
        acc_dot_row := acc_dot_row + dot_queue.bits
        num_words := num_words + UInt(1)

        state := s_inner_test
      }
    }

    is (s_inner_test) {
      when (num_words === io.lhs_cols) {
        acc_dot_row := acc_dot_row << (lbit + rbit)
        num_words := UInt(0)
        
        state := s_inner_post
      }
      .otherwise {
        state := s_inner
      }
    }

    is (s_inner_post) {
      row_res := Mux((UInt(neg_lhs) ^ UInt(neg_rhs)) === UInt(1),
        row_res - acc_dot_row,
        row_res + acc_dot_row)

      rbit := rbit + UInt(1)
      state := s_rbit_test
    }

    is (s_rbit_test) {
      acc_dot_row := UInt(0)
      when (rbit === io.rhs_bits) {
        rbit := UInt(0)
        lbit := lbit + UInt(1)
        state := s_lbit_test
      }
      .otherwise {
        state := s_prep_reader_dynamic0
      }
    }

    is (s_lbit_test) {
      when (lbit === io.lhs_bits) {
        // Put complete row result in the output queue
        out_queue.bits := row_res
        out_queue.valid := Bool(true)

        lbit := UInt(0)
        row_lhs := row_lhs + UInt(1)
        state := s_row_lhs_test
      }
      .otherwise {
        state := s_prep_reader_dynamic0
      }
    }

    is (s_row_lhs_test) {
      row_res := SInt(0)
      when (row_lhs === io.lhs_rows) {
        row_lhs := UInt(0)
        row_rhs := row_rhs + UInt(1)
        state := s_row_rhs_test
      }
      .otherwise {
        state := s_prep_reader_dynamic0
      }
    }

    is (s_row_rhs_test) {
      when (row_rhs === io.rhs_rows) {
        row_rhs := UInt(0)
        chn := chn + UInt(1)
        state := s_chn_test
      }
      .otherwise {
        state := s_prep_reader_dynamic0
      }
    }

    is (s_chn_test) {
      when (chn === io.num_chn) {
        chn := UInt(0)
        state := s_wait
      }
      .otherwise {
        state := s_prep_reader_dynamic0
      }
    }

    is (s_wait) {
      when (io.writer.finished) {
        start_writer := UInt(0)
        state := s_done
      }
    }

    is (s_done) {
      io.done := Bool(true)
      when (!io.start) { state := s_idle }  
    }
  }
}
