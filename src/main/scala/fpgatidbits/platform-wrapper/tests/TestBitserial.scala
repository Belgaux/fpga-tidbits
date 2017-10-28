package fpgatidbits.Testbenches

import Chisel._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.streams._
import fpgatidbits.dma._
import fpgatidbits.ocm._
import fpgatidbits.rosetta._

class TestBitserial(p: PlatformWrapperParams) extends GenericAccelerator(p) {
  // Parameterizable stuff
  val ws = 32
  val W_depth = 2
  val A_depth = 2
  val num_PE = W_depth * A_depth

  val numMemPorts = 0
  val io = new GenericAcceleratorIF(numMemPorts, p) {
    val start = Bool(INPUT)
    val bitplane = Bool(INPUT)
    val done = Bool(OUTPUT)
    val W = SInt(INPUT, width = ws)
    val A = SInt(INPUT, width = ws)
    val out = SInt(OUTPUT, width = ws)
  }

  val s_idle :: s_bitplane :: s_running :: s_accumulate :: s_done :: Nil = Enum(UInt(), 5)
  val state = Reg(init=UInt(s_idle))
  val tmp = Reg(init=SInt(0, width = ws))
  io.out := tmp
  io.done := Bool(false)

  val count = Reg(init=UInt(0, width = log2Up(ws)))
  val planeW = Vec.fill(W_depth) { Reg(init=Bits(0, width = ws)) }
  val planeA = Vec.fill(A_depth) { Reg(init=Bits(0, width = ws)) }

  // PE = Processing Element
  val PE = Vec.fill(num_PE) { Module(new DotEngine(ws)).io }
  val accs = Vec.fill(num_PE) { Reg(init=SInt(0)) }
  val acc_index = Reg(init=UInt(0, width = log2Up(num_PE)))

  for (i <- 0 until W_depth) {
    for (j <- 0 until A_depth) {
      val index = (i * A_depth) + j
      PE(index).din0.bits := planeW(i)
      PE(index).din1.bits := planeA(j)
      PE(index).din0.valid := Bool(false)
      PE(index).din1.valid := Bool(false)
      PE(index).start := Bool(false)
    }
  }

  switch (state) {
    is (s_idle) {
      tmp := SInt(0)
      when (io.start && io.bitplane) { state := s_bitplane }
      .elsewhen (io.start && !io.bitplane) { state := s_running }
    }

    // Pack bitplanes
    is (s_bitplane) {
      for (i <- 0 until W_depth) {
        planeW(i)(count) := io.W(i)
      }
      for (i <- 0 until A_depth) {
        planeA(i)(count) := io.A(i)
      }
      count := count + UInt(1)
      state := s_done
    }

    is (s_running) {
      when (PE(0).done) {
        for (i <- 0 until W_depth) {
          for (j <- 0 until A_depth) {
            val index = (i * A_depth) + j
            val negW = if (i == W_depth - 1) 1 else 0
            val negA = if (j == A_depth - 1) 1 else 0

            // When signs are different it should be negative -> that's an XOR operation
            accs(index) := Mux((UInt(negW) ^ UInt(negA)) === UInt(1),
              accs(index) - (PE(index).dout << UInt(i+j)),
              accs(index) + (PE(index).dout << UInt(i+j)))

            PE(index).start := Bool(false)
          }
        }
        acc_index := UInt(0)
        state := s_accumulate
      }
      .otherwise {
        for (i <- 0 until W_depth) {
          for (j <- 0 until A_depth) {
            val index = (i * A_depth) + j
            PE(index).start := Bool(true)
            PE(index).din0.valid := Bool(true)
            PE(index).din1.valid := Bool(true)
          }
        }
      }
    }

    is (s_accumulate) {
      tmp := tmp + accs(acc_index)
      acc_index := acc_index + UInt(1)
      state := Mux(acc_index === UInt(num_PE) - UInt(1), s_done, s_accumulate)  
    }

    is (s_done) {
      io.done := Bool(true)
      when (!io.start) {
        state := s_idle 
      } 
    }
  }
}
