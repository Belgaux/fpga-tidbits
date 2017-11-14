/*
 * Copyright: 2014-2017, Technical University of Denmark, DTU Compute
 * Author: Martin Schoeberl (martin@jopdesign.com)
 * License: Simplified BSD License
 * 
 * A UART is a serial port, also called an RS232 interface.
 * 
 */

package fpgatidbits.Testbenches

import Chisel._

import fpgatidbits.dma._
import fpgatidbits.streams._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.rosetta._
/*package fpgatidbits.Testbenches

import Chisel._

import fpgatidbits.PlatformWrapper._
import fpgatidbits.rosetta._
*/
/**
 * Transmit part of the UART.
 * A minimal version without any additional buffering.
 * Use an AXI like valid/ready handshake.
 */
class TestUART(p: PlatformWrapperParams, frequency: Int, baudRate: Int) extends GenericAccelerator(p) {
  val numMemPorts = 1
  val io = new GenericAcceleratorIF(numMemPorts, p) {
    val txd = Bits(OUTPUT, 1)
    val data = Bits(INPUT, 8)
    val ready = Bool(OUTPUT)
    val valid = Bool(INPUT)
    val debugOutput = UInt(OUTPUT, width = 64)    
    // oscar sier i fra naar data er klar
    val oscar = Bool(OUTPUT)
    
  }
  val debugReg = Reg(init = UInt(0, width=64))
  io.oscar := Bool(false)
  val BIT_CNT = UInt((frequency + baudRate / 2) / baudRate - 1)
  //printf("I am global\n") 
  val shiftReg = Reg(init = Bits(0x3f))
  val cntReg = Reg(init = UInt(0, 20))
  val bitsReg = Reg(init = UInt(0, 4))

  io.debugOutput := shiftReg
  io.ready := (cntReg === UInt(0)) && (bitsReg === UInt(0))
  io.txd := shiftReg(0)

  // TODO: make the counter a tick generator
  when(cntReg === UInt(0)) {

    cntReg := UInt(BIT_CNT)
    when(bitsReg =/= UInt(0)) {
      val shift = shiftReg >> 1
      shiftReg := Cat(Bits(1), shift(9, 0))
      bitsReg := bitsReg - UInt(1)
    }.otherwise {
      when(io.valid) {
        shiftReg(0) := Bits(0) // start bit
        shiftReg(8, 1) := io.data // data
        shiftReg(10, 9) := Bits(3) // two stop bits
        printf("Bits %d \n", Bits(0))
        printf("io.data %d \n", io.data)
        printf("bits(3) %d \n", Bits(3))
        bitsReg := UInt(11)
        io.oscar := Bool(true)
      }.otherwise {
        shiftReg := Bits(0x3f)
      }
    }

  }.otherwise {
    cntReg := cntReg - UInt(1)
    //printf("cntReg %d \n", cntReg)
  }
}
