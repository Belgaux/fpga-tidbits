package fpgatidbits.rosetta
import Chisel._

// Binary Dot Product Module

class DotProduct(w: Int) extends Module {
  val io = new Bundle {
    val din0  = Decoupled(UInt(width = w)).flip
    val din1  = Decoupled(UInt(width = w)).flip
    val start = Bool(INPUT)
    val done  = Bool(OUTPUT)
    val dout  = Decoupled(UInt(width = w))
  }

  val s_idle :: s_compute :: s_pcnt1 :: s_pcnt2 :: s_done :: Nil = Enum(UInt(), 5)
  val state = Reg(init=UInt(s_idle))
  val tmp = Reg(init=UInt(0, width = w))
  val out = Reg(init=UInt(0, width = w))

  io.din0.ready := Bool(false)
  io.din1.ready := Bool(false)
  io.dout.valid := Bool(false)
  io.dout.bits  := out
  io.done       := Bool(false)

  switch (state) {
    is (s_idle) {
      when (io.start) { state := s_compute }
    }

    is (s_compute) {
      when(io.din0.valid & io.din1.valid) {
        io.din0.ready := Bool(true)
        io.din1.ready := Bool(true)
        tmp := io.din0.bits & io.din1.bits
	state := s_pcnt1
      }
    }

    is (s_pcnt1) {
      out := PopCount(tmp(31,0))
      state := s_pcnt2
    }

    is (s_pcnt2) {
      out := out + PopCount(tmp(63,32))
      state := s_done
    }

    is (s_done) {
      printf("out=%b\n",out)
      io.done := Bool(true)
      io.dout.valid := Bool(true)
      when (!io.start || io.dout.ready) { state := s_idle }
    }
  }
}

