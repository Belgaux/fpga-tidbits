package fpgatidbits.rosetta
import Chisel._

// Binary Dot Product Module

class DotProduct(w: Int) extends Module {
  val io = new Bundle {
    val din0  = Decoupled(UInt(width = w)).flip
    val din1  = Decoupled(UInt(width = w)).flip
    val start = Bool(INPUT)
    val done  = Bool(OUTPUT)
    val dout  = Decoupled(UInt(OUTPUT, width = w))
  }

  val s_idle :: s_compute :: s_pcnt1 :: s_pcnt2 :: s_done :: Nil = Enum(UInt(), 5)
  val state = Reg(init=UInt(s_idle))
  val tmp = Reg(init=UInt(0, width = w))
  val tmp1 = Reg(init=UInt(0, width = 32))
  val tmp2 = Reg(init=UInt(0, width = 32))

  io.din0.ready := Bool(false)
  io.din1.ready := Bool(false)
  io.dout.valid := Bool(false)
  io.dout.bits  := tmp
  io.done       := Bool(false)

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
        state := s_pcnt1
      }
    }

    is (s_pcnt1) {
      if (w == 64) {
        for (i <- 0 until 32) {
          tmp1(i) := tmp(i)
          tmp2(i) := tmp(i+32)
        }
        state := s_pcnt2
      } 
      else {
        tmp := PopCount(tmp)
        state := s_done
      }
    }

    is (s_pcnt2) {
      tmp := PopCount(tmp1) + PopCount(tmp2)
      state := s_done
    }

    is (s_done) {
      io.done := Bool(true)
      io.dout.valid := Bool(true)
      when (!io.start || io.dout.ready) { state := s_idle }
    }
  }
}

