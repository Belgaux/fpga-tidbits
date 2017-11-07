package fpgatidbits.Testbenches

import Chisel._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.dma._
import fpgatidbits.ocm._
import fpgatidbits.rosetta._

class TestBinaryGEMM(p: PlatformWrapperParams) extends GenericAccelerator(p) {

  val word_size = 64
  val num_accs = 256
  val output_queue_size = 8
  val input_queue_size = 8
  val bytes_per_elem = UInt(word_size/8)
  val numMemPorts = 3

  val io = new GenericAcceleratorIF(numMemPorts, p) {
    val addrW = UInt(INPUT, width = 64)
    val addrA = UInt(INPUT, width = 64)
    val addrR = UInt(INPUT, width = 64)
    val byte_count_R = UInt(INPUT, width = 32)
    val W_R = UInt(INPUT, width = 16)
    val W_C = UInt(INPUT, width = 16)
    val A_R = UInt(INPUT, width = 16)
    val A_C = UInt(INPUT, width = 16)
    val w_depth = UInt(INPUT, width = 8)
    val a_depth = UInt(INPUT, width = 8)

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

  io.done := Bool(false)

  //////// REGISTERS
  
  val write_index = Reg(init=UInt(0, width=32))
  val accs = Mem(SInt(width=word_size), num_accs)
  val elems = Reg(init=UInt(0, width = 32))

  // For each row in W, for each column in A
  val row = Reg(init = UInt(0, width = 16))
  val col = Reg(init = UInt(0, width = 16))

  // Bit-plane counters
  val cur_w = Reg(init=UInt(0, width = 16))
  val cur_a = Reg(init=UInt(0, width = 16))

  /////// WIRES

  // Sign bits for bitserial
  // Assume bitplanes are stored in "little endian" order
  val negw = UInt()
  val nega = UInt()
  negw := cur_w === io.w_depth - UInt(1)
  nega := cur_a === io.a_depth - UInt(1)

  val negative = Bool()
  negative := (negw ^ nega) === UInt(1)

  val significance = UInt(width=log2Up(8*8))
  significance := cur_a + cur_w

  // Index for result-matrix accumulators
  val index = UInt()
  index := row * io.A_C + col


  //////// OUTPUT
  
  val sw = Module(new StreamWriter(new StreamWriterParams(
    streamWidth = p.memDataBits, mem = p.toMemReqParams(), chanID = 0
  ))).io
  sw.baseAddr := io.addrR
  sw.byteCount := io.byte_count_R
  sw.start := Bool(false)
  sw.req <> io.memPort(2).memWrReq
  sw.wdat <> io.memPort(2).memWrDat
  sw.rsp <> io.memPort(2).memWrRsp
  plugMemReadPort(2)

  val out_queue = Decoupled(SInt(width=word_size))
  out_queue.valid := Bool(false)
  out_queue.bits := UInt(4095) // debug value, should never happen
  sw.in <> FPGAQueue(out_queue, output_queue_size)


  ///////// INPUT
  
  val plane_w_stride = bytes_per_elem * io.W_C * io.W_R
  val plane_a_stride = bytes_per_elem * io.A_R * io.A_C
  val reader_stride = bytes_per_elem * io.W_C // This is one whole vector of the matrix
  val srW = make_reader(port=0, 
    baseAddr=io.addrW + row * reader_stride + cur_w * plane_w_stride, 
    byteCount = reader_stride, start=Bool(false))
  val srA = make_reader(port=1,
    baseAddr=io.addrA + col * reader_stride + cur_a * plane_a_stride,
    byteCount = reader_stride, start=Bool(false))

  val in_queue_w = FPGAQueue(srW.out, input_queue_size)
  val in_queue_a = FPGAQueue(srA.out, input_queue_size)


  ///////// PROCESSING ELEMENT
  val dot = Module(new DotProduct(word_size)).io
  dot.din0 <> in_queue_w
  dot.din1 <> in_queue_a
  val dot_queue = FPGAQueue(dot.dout, input_queue_size)
  dot_queue.ready := Bool(false)
  val dot_tmp = Reg(init=UInt(0, width=32))
  val acc_tmp = Reg(init=SInt(0, width=word_size))


  ///////// STATE MACHINE

  // TODO: Give these states more descriptive names
  // TODO: Also possible todo, break up the state machine in different modules? Might not be too easy
  val s_idle :: s_buf :: s_compute1 :: s_compute2 :: s_one :: s_two :: s_three :: s_four :: s_five :: s_six :: s_write:: s_done :: s_wait :: Nil = Enum(UInt(), 13)
  val state = Reg(init=UInt(s_idle))

  switch (state) {
    is (s_idle) {
      cur_w       := UInt(0)
      cur_a       := UInt(0)
      row         := UInt(0)
      col         := UInt(0)
      write_index := UInt(0)
      elems       := UInt(0)
      for (i <- 0 until num_accs) {
        accs(i) := SInt(0)  
      }
      when (io.start) {
        state := s_one
      }
    }
    
    is (s_one) {
      when (dot_queue.valid) {
        dot_queue.ready := Bool(true)
        dot_tmp := dot_queue.bits << significance

        elems := elems + UInt(1)
        state := s_compute1
      }
      .otherwise {
        srW.start := Bool(true)
        srA.start := Bool(true)
      }
    }

    is (s_compute1) {
      acc_tmp := accs(index)  
      state := s_compute2
    }

    is (s_compute2) {
      accs(index) := Mux(negative,
        acc_tmp - dot_tmp,
        acc_tmp + dot_tmp)
      state := s_two
    }

    is (s_two) {
      when (elems === io.W_C) { // Done with all elements within one vector mul
        elems := UInt(0)
        col := col + UInt(1)
        state := s_three
      }
      .otherwise { state := s_one }
    }

    is (s_three) {
      when (col === io.A_C) { // Done with all columns for one row
        row := row + UInt(1)
        state := s_four
      }
      .otherwise {
        state := s_one
      }
    }

    is (s_four) {
      when (row === io.W_R) { // Done with all rows
        row := UInt(0)
        col := UInt(0)
        cur_a := cur_a + UInt(1)
        state := s_five
      }
      .otherwise {
        col := UInt(0)
        state := s_one
      }
    }

    is (s_five) {
      when (cur_a === io.a_depth) {
        cur_w := cur_w + UInt(1)
        cur_a := UInt(0)
        state := s_six
      }
      .otherwise {
        state := s_one
      }
    }

    is (s_six) {
      when (cur_w === io.w_depth) {
        write_index := UInt(0)
        state := s_buf
      }
      .otherwise {
        state := s_one  
      }
    }

    is (s_buf) {
      sw.start := Bool(true)
      acc_tmp := accs(write_index)
      state := s_write  
    }

    is (s_write) {
      // start producing and consuming output
      sw.start := Bool(true)
      out_queue.valid := Bool(true)
      when (out_queue.ready) {
        write_index := write_index + UInt(1)
        out_queue.bits := acc_tmp
        when (write_index === ((io.W_R * io.A_C) - UInt(1))) {
          state := s_wait
        }
        .otherwise { state := s_buf }
      }
    }

    is (s_wait) {
      // Wait for StreamWriter to finish writing to DRAM
      sw.start := Bool(true)
      when (sw.finished) { state := s_done }
    }

    is (s_done) {
      io.done := Bool(true)
      when (!io.start) { state := s_idle }  
    }
  }
}
