package fpgatidbits.Testbenches

import Chisel._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.dma._
import fpgatidbits.ocm._
import fpgatidbits.rosetta._

class TestBinaryGEMM(p: PlatformWrapperParams) extends GenericAccelerator(p) {
  val word_size = 64
  val numMemPorts = 0
  val io = new GenericAcceleratorIF(numMemPorts, p) {
    val W = UInt(INPUT, width = word_size)
    val A = UInt(INPUT, width = word_size)
    val W_R = UInt(INPUT, width = 32)
    val W_C = UInt(INPUT, width = 32)
    val A_R = UInt(INPUT, width = 32)
    val A_C = UInt(INPUT, width = 32)
    val W_depth = UInt(INPUT, width = 32)
    val A_depth = UInt(INPUT, width = 32)

    val start = Bool(INPUT)
    val done = Bool(OUTPUT)
    val out = UInt(OUTPUT, width = word_size)
    val write_row = UInt(OUTPUT, width = 32)
    val write_col = UInt(OUTPUT, width = 32)
  }

  // Bit-plane counters
  val cur_w = Reg(init=UInt(0, width = 32))
  val cur_a = Reg(init=UInt(0, width = 32))

  val tmp = Reg(init=SInt(0, width = word_size))
  val bgemm = Module(new BinaryGEMM(word_size)).io
  
  bgemm.W.bits := io.W
  bgemm.W.valid := Bool(true)
  bgemm.A.bits := io.A
  bgemm.A.valid := Bool(true)

  bgemm.start := Bool(false)
  bgemm.W_R := io.W_R
  bgemm.W_C := io.W_C
  bgemm.A_R := io.A_R
  bgemm.A_C := io.A_C
  bgemm.cur_w := cur_w
  bgemm.cur_a := cur_a
  bgemm.max_w := io.W_depth
  bgemm.max_a := io.A_depth
  bgemm.out.ready := Bool(false)
  
  io.done := Bool(false)
  io.out := tmp
  io.write_row := bgemm.write_row
  io.write_col := bgemm.write_col

  val s_idle :: s_one :: s_two :: s_done :: Nil = Enum(UInt(), 4)
  val state = Reg(init=UInt(s_idle))

  switch (state) {
  
    is (s_idle) {
      tmp := SInt(0)
      cur_w := UInt(0)
      cur_a := UInt(0)
      when (io.start) { state := s_one }
    }
    
    is (s_one) {
      bgemm.out.ready := Bool(true)
      when (bgemm.done) {
        printf("bgemm.out.bits=%b\n", bgemm.out.bits)
        printf("bgemm.write_row=%d\n", bgemm.write_row)
        printf("bgemm.write_col=%d\n", bgemm.write_col)
        tmp := bgemm.out.bits
        state := s_done
      }
      .otherwise {
        bgemm.start := Bool(true)
      }
    }

    is (s_two) {
    }
    
    is (s_done) {
      io.done := Bool(true)
      when (!io.start) { state := s_idle }  
    }  
  }
}
