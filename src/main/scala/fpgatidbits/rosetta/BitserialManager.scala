package fpgatidbits.rosetta
import Chisel._

class BitserialManager(word_size : Int, W_depth : Int, A_depth : Int) extends Module {
  
  val io = new Bundle {
    val W = Decoupled(SInt(width = word_size)).flip
    val A = Decoupled(SInt(width = word_size)).flip
    val start = Bool(INPUT)
    val done = Bool(OUTPUT)
    val out = SInt(OUTPUT, width = word_size)
  }

  val s_idle :: s_accumulate :: s_pack :: s_compute :: s_done :: Nil = Enum(UInt(), 5)
  val state = Reg(init = UInt(s_idle))
  val tmp = Reg(init = SInt(0, width = word_size))
  io.out := tmp

  val wsc = Reg(init = UInt(0, width = log2Up(word_size)))
  val PE = Module(new Bitserial(word_size, W_depth, A_depth)).io
  PE.start     := Bool(false)
  PE.bitplane  := Bool(false)
  PE.W         := io.W.bits
  PE.A         := io.A.bits

  io.done := Bool(false)
  io.W.ready := Bool(false)
  io.A.ready := Bool(false)

  switch (state) {
    
    is (s_idle) {
      wsc := UInt(0)
      tmp := SInt(0)
      when (io.start) { state := s_accumulate }
    }
    
    is (s_pack) {
      when (PE.done) {
        wsc := wsc + UInt(1)
        state := Mux(wsc === UInt(word_size) - UInt(1), s_compute, s_accumulate)
      } 
    }

    is (s_compute) {
      when (PE.done) {
        tmp := PE.out
        state := s_done
      }
      .otherwise {
        PE.bitplane := Bool(false)
        PE.start := Bool(true)
      }
    }

    is (s_accumulate) {
      when (io.W.valid && io.A.valid) {
        io.W.ready := Bool(true)
        io.A.ready := Bool(true)
        PE.start := Bool(true)  
        PE.bitplane := Bool(true)
        state := s_pack
      }
    }

    is (s_done) {
      io.done := Bool(true)
      when (!io.start) { state := s_idle }
    }
  
  }

}
