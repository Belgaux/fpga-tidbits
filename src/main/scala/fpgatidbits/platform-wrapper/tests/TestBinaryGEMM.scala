package fpgatidbits.Testbenches

import Chisel._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.dma._
import fpgatidbits.ocm._
import fpgatidbits.rosetta._

class TestBinaryGEMM(p: PlatformWrapperParams) extends GenericAccelerator(p) {
  val word_size = 64
  val num_accs = 1024
  val bytes_per_elem = UInt(word_size/8)
  val numMemPorts = 3
  val io = new GenericAcceleratorIF(numMemPorts, p) {
    val addrW = UInt(INPUT, width = 64)
    val addrA = UInt(INPUT, width = 64)
    val addrR = UInt(INPUT, width = 64)
    val byte_count_R = UInt(INPUT, width = 32)
    val W_R = UInt(INPUT, width = 32)
    val W_C = UInt(INPUT, width = 32)
    val A_R = UInt(INPUT, width = 32)
    val A_C = UInt(INPUT, width = 32)
    val w_depth = UInt(INPUT, width = 32)
    val a_depth = UInt(INPUT, width = 32)

    val start = Bool(INPUT)
    val done = Bool(OUTPUT)
  }

  val row = Reg(init = UInt(0, width = 32))
  val col = Reg(init = UInt(0, width = 32))

  // Bit-plane counters
  val cur_w = Reg(init=UInt(0, width = 32))
  val cur_a = Reg(init=UInt(0, width = 32))

  val negw = Bool()
  val nega = Bool()
  // Assume little endian ordering
  negw := cur_w === io.w_depth - UInt(1)
  nega := cur_a === io.a_depth - UInt(1)

  io.done := Bool(false)
  
  val write_index = Reg(init=UInt(0, width=32))
  val accs = Vec.fill(num_accs) { Reg(init=SInt(0, width = word_size)) }
  val elems = Reg(init=UInt(0, width = 32))

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

  val sw = Module(new StreamWriter(new StreamWriterParams(
    streamWidth = p.memDataBits, mem = p.toMemReqParams(), chanID = 0
  ))).io
  sw.baseAddr := io.addrR
  sw.byteCount := io.byte_count_R
  sw.start := io.start
  sw.req <> io.memPort(2).memWrReq
  sw.wdat <> io.memPort(2).memWrDat
  sw.rsp <> io.memPort(2).memWrRsp
  plugMemReadPort(2)
  sw.in.valid := Bool(false)
  sw.in.bits := accs(write_index)


  val plane_w_stride = bytes_per_elem * io.W_C * io.W_R
  val plane_a_stride = bytes_per_elem * io.A_R * io.A_C
  val reader_stride = bytes_per_elem * io.W_C
  val srW = make_reader(port=0, 
    baseAddr=io.addrW + row * reader_stride + cur_w * plane_w_stride, 
    byteCount = reader_stride, start=Bool(false))
  val srA = make_reader(port=1,
    baseAddr=io.addrA + col * reader_stride + cur_a * plane_a_stride,
    byteCount = reader_stride, start=Bool(false))

  srW.out.ready := Bool(false)
  srA.out.ready := Bool(false)

  val s_idle :: s_one :: s_two :: s_three :: s_four :: s_five :: s_six :: s_write:: s_done :: Nil = Enum(UInt(), 9)
  val state = Reg(init=UInt(s_idle))
  val index = UInt()
  index := row * io.A_C + col


  switch (state) {
  
    is (s_idle) {
      cur_w := UInt(0)
      cur_a := UInt(0)
      row := UInt(0)
      col := UInt(0)
      write_index := UInt(0)
      for (i <- 0 until num_accs) {
        accs(i) := SInt(0)  
      }
      when (io.start) {
        state := s_one
      }
    }
    
    is (s_one) {
      when (srW.out.valid && srA.out.valid) {

        /*
        printf("srW.out.bits=%b\n", srW.out.bits)
        printf("srA.out.bits=%b\n", srA.out.bits)
        printf("index=%d\n", index)
        */

        accs(index) := Mux((UInt(negw) ^ UInt(nega)) === UInt(1),
          accs(index) - (PopCount(srW.out.bits & srA.out.bits) << (cur_w + cur_a)),
          accs(index) + (PopCount(srW.out.bits & srA.out.bits) << (cur_w + cur_a)))


        /*
        printf("cur_w + cur_a = %d\n", cur_w + cur_a)
        printf("accs(%d)=%b\n", index, accs(index))
        printf("Mux output = %b \n", Mux((UInt(negw) ^ UInt(nega)) === UInt(1),
          accs(index) - (PopCount(srW.out.bits & srA.out.bits) << (cur_w + cur_a)),
          accs(index) + (PopCount(srW.out.bits & srA.out.bits) << (cur_w + cur_a))))
        */


        /*
        printf("row=%d\n", row)
        printf("col=%d\n", col)
        */

        srA.out.ready := Bool(true)
        srW.out.ready := Bool(true)
        elems := elems + UInt(1)

        state := s_two
      }
      .otherwise {
        srW.start := Bool(true)
        srA.start := Bool(true)
      }
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
      /*
      for (i <- 0 until 32) {
        printf("accs(%d)=%b\n", UInt(i), accs(i))
      }
      */
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
        state := s_write
      }
      .otherwise {
        state := s_one  
      }
    }

    is (s_write) {
      when (sw.finished) { state := s_done }
      .otherwise {
        sw.in.valid := Bool(true)
        when (sw.in.ready) {
          write_index := write_index + UInt(1)
        }
      }
    }

    is (s_done) {
      io.done := Bool(true)
      when (!io.start) { state := s_idle }  
    }
  }
}
