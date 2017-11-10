package fpgatidbits.rosetta
import Chisel._

class DotProduct(w: Int) extends Module {
  val io = new Bundle {
    val din0  = Decoupled(UInt(width = w)).flip
    val din1  = Decoupled(UInt(width = w)).flip
    val dout  = Decoupled(UInt(width = w))
  }

  val s_compute :: s_pcnt :: s_done :: Nil = Enum(UInt(), 3)
  val state = Reg(init=UInt(s_compute))
  val tmp = Reg(init=UInt(0, width = 32))
  val out = Reg(init=UInt(0, width = w))

  io.din0.ready := Bool(false)
  io.din1.ready := Bool(false)
  io.dout.valid := Bool(false)
  io.dout.bits  := out

  switch (state) {

    is (s_compute) {
      when(io.din0.valid & io.din1.valid) {
        io.din0.ready := Bool(true)
        io.din1.ready := Bool(true)

        // PopCount should only ever take 32 bits
        // PopCount the high 32 bits
        out := PopCount((io.din0.bits & io.din1.bits)(63,32))
        // save the 32 low bits
        tmp := (io.din0.bits & io.din1.bits)(31,0)
	      state := s_pcnt
      }
    }

    is (s_pcnt) {
      // PopCount the remaining bits
      out := out + PopCount(tmp)
      state := s_done
    }

    is (s_done) {
      io.dout.valid := Bool(true)
      when (io.dout.ready) {
        state := s_compute
      }
    }
  }
}

