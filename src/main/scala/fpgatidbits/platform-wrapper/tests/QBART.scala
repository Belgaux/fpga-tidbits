package fpgatidbits.Testbenches

import Chisel._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.dma._
import fpgatidbits.ocm._
import fpgatidbits.rosetta._


class QBART(p: PlatformWrapperParams) extends GenericAccelerator(p) {

  val word_size = 64
  val bytes_per_elem = UInt(word_size/8, width=8)
  val numMemPorts = 3

  val io = new GenericAcceleratorIF(numMemPorts, p) {

    /////// GENERAL IO
    val start = Bool(INPUT)
    val done = Bool(OUTPUT)
    val fc = Bool(INPUT)
    val conv = Bool(INPUT)
    val thresh = Bool(INPUT)

    /////// FULLY CONNECTED IO
    val lhs_addr = UInt(INPUT, width = 64)
    val rhs_addr = UInt(INPUT, width = 64)
    val res_addr = UInt(INPUT, width = 64)

    val lhs_rows = UInt(INPUT, width = 16)
    val lhs_cols = UInt(INPUT, width = 16)
    val lhs_bits = UInt(INPUT, width = 8)
    val lhs_issigned = Bool(INPUT)

    val rhs_rows = UInt(INPUT, width = 16)
    val rhs_cols = UInt(INPUT, width = 16)
    val rhs_bits = UInt(INPUT, width = 8)
    val rhs_issigned = Bool(INPUT)
    val num_chn = UInt(INPUT, width = 16)

    /////// TODO: CONVOLUTION IO


    /////// TODO: THRESHOLDING IO

  }
  

  // Default io

  io.done    := Bool(false)


  // DRAM IO + defaults

  val reader0 = Module(new StreamReader(new StreamReaderParams(
    streamWidth = word_size, fifoElems = 8, mem = p.toMemReqParams(),
    maxBeats = 1, chanID = 0, disableThrottle = true
  ))).io
  reader0.baseAddr   := UInt(0)
  reader0.byteCount  := UInt(0)
  reader0.start      := Bool(false)
  reader0.out.ready  := Bool(false)
  reader0.req <> io.memPort(0).memRdReq
  reader0.rsp <> io.memPort(0).memRdRsp
  plugMemWritePort(0)

  val reader1 = Module(new StreamReader(new StreamReaderParams(
    streamWidth = word_size, fifoElems = 8, mem = p.toMemReqParams(),
    maxBeats = 1, chanID = 0, disableThrottle = true
  ))).io
  reader1.baseAddr   := UInt(0)
  reader1.byteCount  := UInt(0)
  reader1.start      := Bool(false)
  reader1.out.ready  := Bool(false)
  reader1.req <> io.memPort(1).memRdReq
  reader1.rsp <> io.memPort(1).memRdRsp
  plugMemWritePort(1)

  val writer = Module(new StreamWriter(new StreamWriterParams(
    streamWidth = p.memDataBits, mem = p.toMemReqParams(), chanID = 0
  ))).io
  writer.baseAddr := UInt(0)
  writer.byteCount := UInt(0)
  writer.start := Bool(false)
  writer.in.bits := UInt(0)
  writer.in.valid := Bool(false)
  writer.req <> io.memPort(2).memWrReq
  writer.wdat <> io.memPort(2).memWrDat
  writer.rsp <> io.memPort(2).memWrRsp
  plugMemReadPort(2)


  // Layer units

  val fc = Module(new BitserialGEMM(64, p)).io
  fc.start := Bool(false)
  fc.lhs_reader.out.valid := reader0.out.valid
  fc.lhs_reader.out.bits := reader0.out.bits
  fc.rhs_reader.out.valid := reader1.out.valid
  fc.rhs_reader.out.bits := reader1.out.bits
  fc.writer.finished := writer.finished
  fc.writer.in.ready := writer.in.ready
  fc.lhs_addr := io.lhs_addr
  fc.rhs_addr := io.rhs_addr
  fc.res_addr := io.res_addr
  fc.lhs_rows := io.lhs_rows
  fc.lhs_cols := io.lhs_cols
  fc.lhs_bits := io.lhs_bits
  fc.lhs_issigned := io.lhs_issigned
  fc.rhs_rows := io.rhs_rows
  fc.rhs_cols := io.rhs_cols
  fc.rhs_bits := io.rhs_bits
  fc.rhs_issigned := io.rhs_issigned
  fc.num_chn := io.num_chn


  // This state machine rewires readers/writers to running layers

  val s_idle :: s_fc :: s_conv :: s_thresh :: s_done :: Nil = Enum(UInt(), 5)
  val state = Reg(init=UInt(s_idle))

  switch (state) {
    
    is (s_idle) {
      when (io.start) {
        when      (io.fc)     { state := s_fc }
        .elsewhen (io.conv)   { state := s_conv }
        .elsewhen (io.thresh) { state := s_thresh }
      }
    }
    
    is (s_fc) {
      when (fc.done) {
        state := s_done
      }
      .otherwise {
        reader0.baseAddr := fc.lhs_reader.baseAddr
        reader0.byteCount := fc.lhs_reader.byteCount
        reader0.start := fc.lhs_reader.start
        reader0.out.ready := fc.lhs_reader.out.ready

        reader1.baseAddr := fc.rhs_reader.baseAddr
        reader1.byteCount := fc.rhs_reader.byteCount
        reader1.start := fc.rhs_reader.start
        reader1.out.ready := fc.rhs_reader.out.ready

        writer.baseAddr := fc.writer.baseAddr
        writer.byteCount := fc.writer.byteCount
        writer.start := fc.writer.start
        writer.in.bits := fc.writer.in.bits
        writer.in.valid := fc.writer.in.valid

        fc.start := Bool(true)
      }
    }
    
    is (s_conv) {
    
    }
    
    is (s_thresh) {
    
    }
    
    is (s_done) {
      io.done := Bool(true)
      when (!io.start) { state := s_idle }
    }  
  }
}
