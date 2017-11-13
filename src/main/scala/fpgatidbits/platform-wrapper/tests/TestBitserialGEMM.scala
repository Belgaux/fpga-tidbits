package fpgatidbits.Testbenches

import Chisel._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.dma._
import fpgatidbits.ocm._
import fpgatidbits.rosetta._

// Right-hand side must be given transposed

class TestBitserialGEMM(p: PlatformWrapperParams) extends GenericAccelerator(p) {

  val word_size = 64
  val bytes_per_elem = UInt(word_size/8, width=8)
  val numMemPorts = 3

  val io = new GenericAcceleratorIF(numMemPorts, p) {
    val lhs_addr = UInt(INPUT, width = 64)
    val rhs_addr = UInt(INPUT, width = 64)
    val res_addr = UInt(INPUT, width = 64)
    val res_byte_count = UInt(INPUT, width = 32)

    val lhs_rows = UInt(INPUT, width = 16)
    val lhs_cols = UInt(INPUT, width = 16)
    val lhs_bits = UInt(INPUT, width = 8)
    val lhs_issigned = Bool(INPUT)

    val rhs_rows = UInt(INPUT, width = 16)
    val rhs_cols = UInt(INPUT, width = 16)
    val rhs_bits = UInt(INPUT, width = 8)
    val rhs_issigned = Bool(INPUT)

    val num_chn = UInt(INPUT, width = 16)

    val start = Bool(INPUT)
    val done = Bool(OUTPUT)
  }
  
  val gemm = Module(new BitserialGEMM(64, p)).io
  gemm.start := io.start
  io.done    := gemm.done

  gemm.lhs_addr := io.lhs_addr
  gemm.rhs_addr := io.rhs_addr
  gemm.res_addr := io.res_addr
  gemm.res_byte_count := io.res_byte_count
  gemm.num_chn := io.num_chn

  gemm.lhs_rows := io.lhs_rows
  gemm.lhs_cols := io.lhs_cols
  gemm.lhs_bits := io.lhs_bits
  gemm.lhs_issigned := io.lhs_issigned

  gemm.rhs_rows := io.rhs_rows
  gemm.rhs_cols := io.rhs_cols
  gemm.rhs_bits := io.rhs_bits
  gemm.rhs_issigned := io.rhs_issigned

  val lsr = Module(new StreamReader(new StreamReaderParams(
    streamWidth = word_size, fifoElems = 8, mem = p.toMemReqParams(),
    maxBeats = 1, chanID = 0, disableThrottle = true
  ))).io
  lsr <> gemm.lhs_reader
  lsr.req <> io.memPort(0).memRdReq
  lsr.rsp <> io.memPort(0).memRdRsp
  plugMemWritePort(0)
  
  val rsr = Module(new StreamReader(new StreamReaderParams(
    streamWidth = word_size, fifoElems = 8, mem = p.toMemReqParams(),
    maxBeats = 1, chanID = 0, disableThrottle = true
  ))).io
  rsr <> gemm.rhs_reader
  rsr.req <> io.memPort(1).memRdReq
  rsr.rsp <> io.memPort(1).memRdRsp
  plugMemWritePort(1)

  val sw = Module(new StreamWriter(new StreamWriterParams(
    streamWidth = p.memDataBits, mem = p.toMemReqParams(), chanID = 0
  ))).io
  sw <> gemm.writer
  sw.req <> io.memPort(2).memWrReq
  sw.wdat <> io.memPort(2).memWrDat
  sw.rsp <> io.memPort(2).memWrRsp
  plugMemReadPort(2)

}
