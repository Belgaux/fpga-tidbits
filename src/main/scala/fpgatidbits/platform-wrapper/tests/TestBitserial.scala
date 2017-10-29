package fpgatidbits.Testbenches

import Chisel._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.dma._
import fpgatidbits.streams._
import fpgatidbits.rosetta._

class TestBitserial(p: PlatformWrapperParams) extends GenericAccelerator(p) {
  val word_size = 32
  val numMemPorts = 3
  val io = new GenericAcceleratorIF(numMemPorts, p) {
    val start = Bool(INPUT)
    val addrW = UInt(INPUT, width = 64)
    val addrA = UInt(INPUT, width = 64)
    val addrR = UInt(INPUT, width = 64)
    val W_R = UInt(INPUT, width = 32)
    val W_C = UInt(INPUT, width = 32)
    val A_R = UInt(INPUT, width = 32)
    val A_C = UInt(INPUT, width = 32)
    val byte_count_W = UInt(INPUT, width = 32)
    val byte_count_A = UInt(INPUT, width = 32)
    val byte_count_R = UInt(INPUT, width = 32)
    val done = Bool(OUTPUT)
  }

  def make_reader(port: Int, baseAddr : UInt, byteCount : UInt) = {
    val sr = Module(new StreamReader(new StreamReaderParams(
      streamWidth = word_size, fifoElems = 8, mem = p.toMemReqParams(),
      maxBeats = 1, chanID = 0, disableThrottle = true
    ))).io
    sr.start := Bool(false)
    sr.baseAddr := baseAddr
    sr.byteCount := byteCount
    sr.req <> io.memPort(port).memRdReq
    sr.rsp <> io.memPort(port).memRdRsp
    plugMemWritePort(port)
    sr
  }

  val srW = make_reader(port=0, baseAddr=io.addrW, byteCount=io.byte_count_W)
  val srA = make_reader(port=1, baseAddr=io.addrA, byteCount=io.byte_count_A)

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

  val acc = Reg(init = SInt(0, width = word_size))
  val row_count = Reg(init = UInt(0, width = 32))
  val col_count = Reg(init = UInt(0, width = 32))

  // Track how many elems we've computed from the input stream for one VV pair
  val elems = Reg(init = UInt(0, width = 32))

  // BitserialManager eats word-size amount of input elements
  val M = Module(new BitserialManager(word_size, 2, 2)).io
  M.start := Bool(false)
  io.done := Bool(false)

  srW.out <> M.W
  srA.out <> M.A

  sw.in.valid := Bool(false)
  sw.in.bits := acc

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
      when (row_count === io.W_R) {
        state := s_done
      }
      .elsewhen (row_count != io.W_R
                  && col_count === io.A_C) {
        row_count := row_count + UInt(1)
        col_count := UInt(0)
        acc := UInt(0)
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
      when (M.done) {
        acc := acc + M.out
        elems := elems + UInt(word_size)
        state := s_col
      }
      .otherwise {
        M.start := Bool(true)  
        srW.start := Bool(true)
        srA.start := Bool(true)
      } 
    }

    is (s_write) {
      sw.in.valid := Bool(true)
      when (sw.in.ready) {
        state := s_row
      }
    }

    is (s_done) {
      io.done := Bool(true)
      when (!io.start) { state := s_idle }
    }
  }
}
