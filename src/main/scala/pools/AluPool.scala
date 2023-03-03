package pools

import chisel3._
import chisel3.util._

//configurable
val PRegNUm = 128
val BidNUm = 32
val SidNUm = 32
val LidNum = 32
val IidNum = 128

val PRegWidth = log2Up(PRegNum)
val LSidNum = LidNum + SidNum
val BSidNum = BidNum + SidNum
val LidWidth = log2Up(LidNum) 
val SidWidth = log2Up(SidNum) 
val BidWidth = log2Up(BidNum) 
val BSidWidth = log2Up(BSidNum) 
val LSidWidth = log2Up(LSidNum) 


//alu + load + branch
class AluPool (numEntriesEachInst: Int = 8) extends Module {
  val PoolInNum  = 8   //pool input number, 8 dispatch in, not configurable
  val PoolOutNum = 4   //pool output number, 4 issue out,  not configurable
  val RFReadNum  = 4   //regfile read number, not configurable
  val WakeNum    = 8   //wakeup number, not configurable
  val numEntries = numEntriesEachInst * PoolInNum 
  val numEntriesIn = numEntriesEachInst * 2
  val numEntriesOut = numEntriesEachInst * 4
  val io = IO(new Bundle {
    val poolFull = Output(Bool()) 
    val poolId = Output(Vec(PoolInNum, UInt(log2Up(numEntries).W)))
    val in = Input(Vec(PoolInNum, new MicroOp()))  
    val regReq = Output(Vec(RFReadNum, UInt(PhyRegWidth.W)))
    val regRsp = Input(Vec(RFReadNum, UInt(64.W))) 
    val out = Output(Vec(PoolOutNum, new MicroOp())) 
    val wakeOut = Output(Vec(PoolOutNum, new MicroOp())) 
    val wakeIn = Input(Vec(WakeNum, new MicroOp()))
    val dataIn = Input(Vec(WakeNum, UInt(64.W)))
    
  })
  val in = io.in
  val regRsp = io.regRsp
  val wakeIn = io.wakeIn
  val dataIn = io.dataIn

  //reg
  val src1Rdy  = RegInit(0.U(numEntries.W))
  val src1DRdy = RegInit(0.U(numEntries.W))
  val src1Data = Reg(Vec(numEntries, UInt(64.W)))
  
  val src2Rdy  = RegInit(0.U(numEntries.W))
  val src2DRdy = RegInit(0.U(numEntries.W))
  val src2Data = Reg(Vec(numEntries, UInt(64.W)))

  val valid = RegInit(0.U(numEntries.W)
  val isAl     = RegInit(0.U(numEntries.W)
  val isLd     = RegInit(0.U(numEntries.W)
  val isBr     = RegInit(0.U(numEntries.W)
  val iid      = Reg(Vec(numEntries, UInt(IidWidth.W)))
  val lbid     = Reg(Vec(numEntries, UInt(max(BidWidth, LidWidth).W)))
  val src1Addr = Reg(Vec(numEntries, UInt(PhyRegWidth.W)))
  val src2Addr = Reg(Vec(numEntries, UInt(PhyRegWidth.W)))
  val dstAddr  = Reg(Vec(numEntries, UInt(PhyRegWidth.W)))
  
  val poolFull = RegInit(false.B)
  val outPid = RegInit(Vec(PoolOutNum, 0.U(log2Up(numEntries).W)))

  //wire
  val inPid    = Wire(Vec(PoolInNum , UInt(log2Up(numEntries).W)))
  val validNxt = Wire(Vec(numEntries, Bool()))
  val isAlNxt  = Wire(Vec(numEntries, Bool()))
  val isLdNxt  = Wire(Vec(numEntries, Bool()))
  val isBrNxt  = Wire(Vec(numEntries, Bool()))

  //logic
  poolFull := (0 untill PoolInNum/2).map(
              PopCount(validNxt((_+1)*numEntriesIn-1, _*numEntriesIn) > numEntriesIn-4)
              ).reduce(_ | _) 

  for(i <- 0 until PoolInNum/2) {
    inPid(  i) := PriorityEncoder(~valid((i+1)*numEntriesIn-1, i*numEntriesIn)) + i*numEntriesIn  
    inPid(7-i) := numEntriesIn - 1 - PriorityEncoder(Reverse(~valid((i+1)*numEntriesIn-1, i*numEntriesIn))) + i*numEntriesIn
  }

  valid := validNxt.asUInt
  isAl := isAlNxt.asUInt
  isBr := isBrNxt.asUInt
  isLs := isLsNxt.asUInt

  for (i <- 0 until numEntries) {
    val inId0 = i / inumEntriesIn
    val inId1 = PoolInNum - 1 - i/numEntriesIn
    val outId0 = i / numEntriesOut * 2
    val outId1 = i / numEntriesOut * 2 + 1
    validNxt(i) := (valid(i) || in ( inId0).valid && UIntToOH(in ( inId0).pid)(i) || in ( inId1).valid && UIntToOH(in ( inId1).pid)(i)) && !(
                                out(outId0).valid && UIntToOH(out(outId0).pid)(i) || out(outId1).valid && UIntToOH(out(outId1).pid)(i))
    isAlNxt(i)  := (isAl(i ) || in ( inId0).isAl  && UIntToOH(in ( inId0).pid)(i) || in ( inId1).isAl  && UIntToOH(in ( inId1).pid)(i)) && !(
                                out(outId0).isAl  && UIntToOH(out(outId0).pid)(i) || out(outId1).isAl  && UIntToOH(out(outId1).pid)(i))
    isBrNxt(i)  := (isBr(i ) || in ( inId0).isBr  && UIntToOH(in ( inId0).pid)(i) || in ( inId1).isBr  && UIntToOH(in ( inId1).pid)(i)) && !(
                                out(outId0).isBr  && UIntToOH(out(outId0).pid)(i) || out(outId1).isBr  && UIntToOH(out(outId1).pid)(i))
    isLdNxt(i)  := (isLd(i ) || in ( inId0).isLd  && UIntToOH(in ( inId0).pid)(i) || in ( inId1).isLd  && UIntToOH(in ( inId1).pid)(i)) && !(
                                out(outId0).isLd  && UIntToOH(out(outId0).pid)(i) || out(outId1).isLd  && UIntToOH(out(outId1).pid)(i))
  } 
   

  for (i <- 0 until numEntries) {
    val inId0 = i/numEntriesIn
    val inId1 = PoolInNum - 1 - i/numEntriesIn
    when (in(inId0).pid === i) {
      iid(i)  := in(inId0).iid
      lbid(i) := Mux(in(inId0).isLd, in(inId0).lid, in(inId0).bid)
      src1Addr(i)  := in(inId0).src1Addr
      src2Addr(i)  := in(inId0).src2Addr
      dstAddr(i)   := in(inId0).src2Addr
    }.elsewhen(in(inId1).pid == i) {
      iid(i)  := in(inId1).iid
      lbid(i) := Mux(in(inId1).isLd, in(inId1).lid, in(inId1).bid)
      src1Addr(i)  := in(inId1).src1Addr
      src2Addr(i)  := in(inId1).src2Addr
      dstAddr(i)   := in(inId1).dstAddr
    }
  } 
   
 

  
}
