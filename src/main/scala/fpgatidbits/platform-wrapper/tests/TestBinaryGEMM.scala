package fpgatidbits.Testbenches

import Chisel._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.dma._
import fpgatidbits.ocm._
import fpgatidbits.rosetta._

// Right-hand side must be given transposed

class TestBinaryGEMM(p: PlatformWrapperParams) extends GenericAccelerator(p) {

  val word_size = 64
  val output_queue_size = 8
  val input_queue_size = 8
  val bytes_per_elem = UInt(word_size/8)

  // This has to be camel-case for some reason??
  val numMemPorts = 3

  val io = new GenericAcceleratorIF(numMemPorts, p) {
    val lhs_addr = UInt(INPUT, width = 64)
    val rhs_addr = UInt(INPUT, width = 64)
    val res_addr = UInt(INPUT, width = 64)
    val res_byte_count = UInt(INPUT, width = 32)

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
  }

  def make_reader(port: Int, baseAddr : UInt, byteCount : UInt, start:Bool) = {
    val sr = Module(new StreamReader(new StreamReaderParams(
      streamWidth = word_size, fifoElems = 8, mem = p.toMemReqParams(),
      maxBeats = 1, chanID = 0, disableThrottle = true
    ))).io
    sr.start := start
    sr.baseAddr := baseAddr
    sr.byteCount := byteCount
    sr.req <> io.memPort(port).memRdReq
    sr.rsp <> io.memPort(port).memRdRsp
    plugMemWritePort(port)
    sr
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

  // Accumulate result of all bitplanes for each row-pair
  val row_res = Reg(init=SInt(0, width=word_size))
  // Acuumulate AND-PCNT result within each bitplane row-pair
  val acc_dot_row = Reg(init=UInt(0, width=word_size))


  /////// WIRES

  // Sign-bits for bitserial algorithm
  // Assume that bitplanes are stored in "little endian" order
  val neg_lhs = Bool()
  val neg_rhs = Bool()
  neg_lhs := io.lhs_issigned && (lbit === (io.lhs_bits - UInt(1)))
  neg_rhs := io.rhs_issigned && (rbit === (io.rhs_bits - UInt(1)))

  io.done := Bool(false)

  //////// OUTPUT
 
  val sw = Module(new StreamWriter(new StreamWriterParams(
    streamWidth = p.memDataBits, mem = p.toMemReqParams(), chanID = 0
  ))).io
  sw.baseAddr := io.res_addr
  sw.byteCount := io.res_byte_count
  sw.start := io.start
  sw.req <> io.memPort(2).memWrReq
  sw.wdat <> io.memPort(2).memWrDat
  sw.rsp <> io.memPort(2).memWrRsp
  plugMemReadPort(2)

  val out_queue = Decoupled(SInt(width=word_size))
  out_queue.valid := Bool(false)
  out_queue.bits := UInt(666) // debug value, should never happen
  sw.in <> FPGAQueue(out_queue, output_queue_size)


  ///////// INPUT

  // TODO: Optimize these multiplications
  val lhs_bytes_per_bitplane = bytes_per_elem * io.lhs_cols * io.lhs_rows
  val rhs_bytes_per_bitplane = bytes_per_elem * io.rhs_rows * io.rhs_cols
  val bytes_per_row = bytes_per_elem * io.lhs_cols

  val lhs_reader = make_reader(port=0, 
    baseAddr=io.lhs_addr + row_lhs * bytes_per_row + lbit * lhs_bytes_per_bitplane,
    byteCount=bytes_per_row, start=Bool(false))

  val rhs_reader = make_reader(port=1,
    baseAddr=io.rhs_addr + row_rhs * bytes_per_row + rbit * rhs_bytes_per_bitplane,
    byteCount=bytes_per_row, start=Bool(false))

  def start_readers() = {
    lhs_reader.start := Bool(true)
    rhs_reader.start := Bool(true)  
  }

  ///////// PROCESSING ELEMENT

  val dot = Module(new DotProduct(word_size)).io
  dot.din0 <> FPGAQueue(lhs_reader.out, input_queue_size)
  dot.din1 <> FPGAQueue(rhs_reader.out, input_queue_size)
  val dot_queue = FPGAQueue(dot.dout, input_queue_size)
  dot_queue.ready := Bool(false)


  ///////// STATE MACHINE

  val s_idle :: s_inner :: s_inner_test :: s_inner_post :: s_rbit_test :: s_lbit_test :: s_row_lhs_test :: s_row_rhs_test :: s_wait :: s_done :: Nil = Enum(UInt(), 10)
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

        start_readers()
        state := s_inner
      }
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
        start_readers()
        state := s_inner
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
        start_readers()
        state := s_inner
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
        start_readers()
        state := s_inner
      }
    }

    is (s_row_rhs_test) {
      when (row_rhs === io.rhs_rows) {
        row_rhs := UInt(0)
        state := s_wait
      }
      .otherwise {
        start_readers()
        state := s_inner
      }
    }

    is (s_wait) {
      // Wait for StreamWriter to finish writing to DRAM
      when (sw.finished) {
        state := s_done
      }
    }

    is (s_done) {
      io.done := Bool(true)
      when (!io.start) { state := s_idle }  
    }
  }
}
