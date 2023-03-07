package tools

import chisel3._
import chisel3.util._

/*
case object CoreParametersKey extends Field[CoreParameters]
case class CoreParameters (
  VAddrBits : Int = 39,
  PRegNUm   : Int = 128,
  IidNum    : Int = 128,
  BidNUm    : Int = 32,
  LidNum    : Int = 32,
  SidNum    : Int = 32,
){
  val PRegWidth = log2Up(PRegNum)
  val IidWidth = log2Up(IidNum)
  val BidWidth = log2Up(BidNum) 
  val LidWidth = log2Up(LidNum) 
  val SidWidth = log2Up(SidNum) 
}
*/
trait CoreParameters {
  val VAddrBits = 39
  val PRegNum = 128
  val IidNum = 32
  val BidNum = 32
  val LidNum =32
  val SidNum = 32

  val PRegWidth = log2Up(PRegNum)
  val IidWidth = log2Up(IidNum)
  val BidWidth = log2Up(BidNum) 
  val LidWidth = log2Up(LidNum) 
  val SidWidth = log2Up(SidNum) 
}

class MicroOp extends Bundle with CoreParameters{
  val valid    = Bool()
  val iid      = UInt(IidWidth.W) 
  val bid      = UInt(BidWidth.W)
  val lid      = UInt(LidWidth.W)
  val sid      = UInt(SidWidth.W)
  val pc       = UInt(VAddrBits.W)
  val src1Vld  = Bool()
  val src1Rdy  = Bool()
  val src1Addr = UInt(PRegWidth.W) 
  val src1Data = UInt(64.W)
  val src2Vld  = Bool()
  val src2Rdy  = Bool()
  val src2Addr = UInt(PRegWidth.W) 
  val src2Data = UInt(64.W)
  val dstAddr  = UInt(PRegWidth.W) 
}

class Redirect extends Bundle with CoreParameters {
  val flush = Bool()
  val brRBK = Bool()
  val flushedBid = UInt(BidNum.W) 
  val ldRBK = Bool()
  val flushedLid = UInt(LidNum.W) 
}
