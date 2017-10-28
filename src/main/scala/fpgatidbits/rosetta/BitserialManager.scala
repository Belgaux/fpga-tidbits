package fpgatidbits.rosetta
import Chisel._

class BitserialManager() extends Module {
  
  val io = new Bundle {
    val A = Decoupled(SInt()).flip
    val B = Decoupled(SInt()).flip
  }  
  
  
}
