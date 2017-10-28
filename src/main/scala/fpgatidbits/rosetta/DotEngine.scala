package fpgatidbits.rosetta
import Chisel._

// Binary Dot Product Module

class DotEngine(w: Int) extends Module {
  val io = new Bundle {
    val din0 = Decoupled(UInt(width = w)).flip
    val din1 = Decoupled(UInt(width = w)).flip
    val start = Bool(INPUT)
    val done = Bool(OUTPUT)
    val dout = UInt(OUTPUT, width = w)
  }

  val s_idle :: s_compute :: s_pcnt :: s_done :: Nil = Enum(UInt(), 4)
  val state = Reg(init=UInt(s_idle))
  val tmp = Reg(init=UInt(0, width = w))

  io.din0.ready := Bool(false)
  io.din1.ready := Bool(false)
  io.dout := tmp
  io.done := Bool(false)
  
  switch (state) {
    is (s_idle) {
      tmp := UInt(0)
      when (io.start) { state := s_compute }
    }

    is (s_compute) {
      when(io.din0.valid & io.din1.valid) {
        io.din0.ready := Bool(true)
        io.din1.ready := Bool(true)
        tmp := io.din0.bits & io.din1.bits
        state := s_pcnt
      }
    }

    is (s_pcnt) {
      tmp := PopCount(tmp)
      state := s_done
    }

    is (s_done) {
      io.done := Bool(true)
      when (!io.start) { state := s_idle }
    }
  }
}

