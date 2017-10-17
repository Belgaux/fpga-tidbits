package fpgatidbits.Testbenches

import Chisel._
import fpgatidbits.dma._
import fpgatidbits.streams._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.rosetta._

class TestDRAM(p: PlatformWrapperParams) extends GenericAccelerator(p) {
  val numMemPorts = 3
  val io = new GenericAcceleratorIF(numMemPorts, p) {
    val start = Bool(INPUT)
    val finished = Bool(OUTPUT)
    val addrA = UInt(INPUT, width=64)
    val addrB = UInt(INPUT, width=64)
    val addrR = UInt(INPUT, width=64)
    val out = UInt(OUTPUT, width=64)
    val count = UInt(INPUT, width=32)
    val byteCount = UInt(INPUT, width=32)
  }

  val reader1 = Module(new StreamReader(new StreamReaderParams(
    streamWidth = 32, fifoElems = 8, mem = p.toMemReqParams(),
    maxBeats = 1, chanID = 0, disableThrottle = true
  ))).io
  val reader2 = Module(new StreamReader(new StreamReaderParams(
    streamWidth = 32, fifoElems = 8, mem = p.toMemReqParams(),
    maxBeats = 1, chanID = 0, disableThrottle = true
  ))).io

  reader1.start := io.start
  reader2.start := io.start

  reader1.baseAddr := io.addrA
  reader2.baseAddr := io.addrB
  reader1.byteCount := io.byteCount
  reader2.byteCount := io.byteCount

  reader1.req <> io.memPort(0).memRdReq
  reader2.req <> io.memPort(1).memRdReq
  io.memPort(0).memRdRsp <> reader1.rsp
  io.memPort(1).memRdRsp <> reader2.rsp
  plugMemWritePort(0)
  plugMemWritePort(1)

  val bvv = Module(new BinaryVecVec(32)).io
  bvv.start := io.start
  bvv.byte_count := io.byteCount
  io.out := bvv.out.bits

  reader1.out <> bvv.a
  reader2.out <> bvv.b
  
  val wr = Module(new StreamWriter(new StreamWriterParams(
    streamWidth = p.memDataBits, mem = p.toMemReqParams(), chanID = 0
  ))).io

  wr.byteCount := io.count * UInt(p.memDataBits/8)
  wr.baseAddr := io.addrR
  wr.start := io.start
  wr.req <> io.memPort(2).memWrReq
  wr.wdat <> io.memPort(2).memWrDat
  io.memPort(2).memWrRsp <> wr.rsp
  plugMemReadPort(2)

  // All we use this for is to generate a decoupled interface 
  // on the number of elements to write to DRAM
  // Look at the source for StreamRepeatElem to see why
  val sg = Module(new SequenceGenerator(p.memDataBits)).io
  sg.start := io.start
  sg.init := io.count
  sg.step := UInt(0)
  sg.count := UInt(1)

  // We stream bvv.out, "io.count" number of times and connect it to
  // the StreamWriter
  StreamRepeatElem(bvv.out, sg.seq) <> wr.in

  io.finished := wr.finished
  io.signature := makeDefaultSignature()
}

