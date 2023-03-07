package pools

import chisel3._
import chisel3.util._

import tools._


//alu + load + branch
class AluPool (numEntriesEachInst: Int = 8) extends Module with CoreParameters{
  val PoolInNum  = 8   //pool input number, 8 dispatch in, not configurable
  val PoolOutNum = 4   //pool output number, 4 issue out,  not configurable
  val RFReadNum  = 4   //regfile read number, not configurable
  val WakeNum    = 8   //wakeup number, not configurable
  val LdNum      = 4
  val NumEntries = numEntriesEachInst * PoolInNum 
  val NumEntriesIn = numEntriesEachInst * 2
  val NumEntriesOut = numEntriesEachInst * 4
  val PidWidth = log2Up(NumEntries)
  val io = IO(new Bundle {
    val redirect = Input(new Redirect) 
    val poolFull = Output(Bool()) 
    val poolId = Output(Vec(PoolInNum, UInt(PidWidth.W)))
    val in = Input(Vec(PoolInNum, new MicroOp()))  
    val regReq = Output(Vec(RFReadNum, UInt(PRegWidth.W)))
    val regRsp = Input(Vec(RFReadNum, UInt(64.W))) 
    val out = Output(Vec(PoolOutNum, new MicroOp())) 
    val wakeOut = Output(Vec(PoolOutNum, new MicroOp())) 
    val wakeIn = Input(Vec(WakeNum, new MicroOp()))
    val dataIn = Input(Vec(WakeNum, UInt(64.W)))
    //val ldReq = Output(Vec(LdNum, new LdReq()))
    //val ldRsp = Input(Vec(LdNum, new LdRsp))
  })

  //input port
  val in = io.in
  val regRsp = io.regRsp
  val wakeIn = io.wakeIn
  val dataIn = io.dataIn
  val redirect = io.redirect
  //val ldRsp = io.ldRsp
  //output port
  //reg
  val valid        = RegInit(Vec(NumEntries, false.B))
  val isAl         = RegInit(Vec(NumEntries, false.B))
  val isLd         = RegInit(Vec(NumEntries, false.B))
  val isBr         = RegInit(Vec(NumEntries, false.B))
  val iid          = Reg    (Vec(NumEntries, UInt(IidWidth.W)))
  val lid          = Reg    (Vec(NumEntries, UInt(LidWidth.W)))
  val bid          = Reg    (Vec(NumEntries, UInt(BidWidth.W)))
  val src1Addr     = Reg    (Vec(NumEntries, UInt(PRegWidth.W)))
  val src2Addr     = Reg    (Vec(NumEntries, UInt(PRegWidth.W)))
  val dstAddr      = Reg    (Vec(NumEntries, UInt(PRegWidth.W)))
  
  val src1Rdy      = RegInit(Vec(NumEntries, false.B))
  val src1DRdy     = RegInit(Vec(NumEntries, false.B))
  val src1FwdVld_0 = RegInit(Vec(NumEntries, false.B))
  val src1FwdVld_1 = RegInit(Vec(NumEntries, false.B))
  val src1FwdSrc_0 = RegInit(Vec(NumEntries, 0.U(WakeNum)))
  val src1FwdSrc_1 = RegInit(Vec(NumEntries, 0.U(WakeNum)))
  val src1Data     = Reg    (Vec(NumEntries, UInt(64.W)))
  
  val src2Rdy      = RegInit(Vec(NumEntries, false.B))
  val src2DRdy     = RegInit(Vec(NumEntries, false.B))
  val src2FwdVld_0 = RegInit(Vec(NumEntries, false.B))
  val src2FwdVld_1 = RegInit(Vec(NumEntries, false.B))
  val src2FwdSrc_0 = RegInit(Vec(NumEntries, 0.U(WakeNum)))
  val src2FwdSrc_1 = RegInit(Vec(NumEntries, 0.U(WakeNum)))
  val src2Data     = Reg    (Vec(NumEntries, UInt(64.W)))

  val poolFull     = RegInit(false.B)
  val inPid       = RegInit(Vec(PoolOutNum, 0.U(PidWidth.W)))
  //wire
  val out      = Wire(Vec(PoolOutNum, new MicroOp))
  val validNxt = Wire(Vec(NumEntries, Bool()))
  

  //logic
  poolFull := (0 until PoolInNum/2).map( i => 
              PopCount(validNxt.asUInt(( i +1)*NumEntriesIn-1, i * NumEntriesIn)) > (NumEntriesIn-4).asUInt
              ).reduce(_ | _) 

  for(i <- 0 until PoolInNum/2) {
    inPid(  i) := PriorityEncoder(~valid((i+1)*NumEntriesIn-1, i*NumEntriesIn)) + i*NumEntriesIn  
    inPid(7-i) := NumEntriesIn - 1 - PriorityEncoder(Reverse(~valid((i+1)*NumEntriesIn-1, i*NumEntriesIn))) + i*NumEntriesIn
  }

  //ctrlSignal update
  for (i <- 0 until NumEntries) {
    val inId0 = i / NumEntriesIn
    val inId1 = PoolInNum - 1 - i/NumEntriesIn
    val outId0 = i / NumEntriesOut * 2
    val outId1 = i / NumEntriesOut * 2 + 1
    validNxt(i) := (valid(i) || 
                 in ( inId0).valid && UIntToOH(in ( inId0).pid)(i) || 
                 in ( inId1).valid && UIntToOH(in ( inId1).pid)(i)) && !(
                 out(outId0).valid && UIntToOH(out(outId0).pid)(i) || 
                 out(outId1).valid && UIntToOH(out(outId1).pid)(i) ||
                 redirect.brRBK && redirect.flushedBid(bid(i))  ||
                 redirect.ldRBK && redirect.flushedLid(lid(i))  ||
                 redirect.flush)
    valid(i) := validNxt
    isAl(i)  := (isAl(i ) || 
                 in ( inId0).isAl  && UIntToOH(in ( inId0).pid)(i) || 
                 in ( inId1).isAl  && UIntToOH(in ( inId1).pid)(i)) && !(
                 out(outId0).valid && UIntToOH(out(outId0).pid)(i) || 
                 out(outId1).valid && UIntToOH(out(outId1).pid)(i) ||
                 redirect.brRBK && redirect.flushedBid(bid(i))  ||
                 redirect.ldRBK && redirect.flushedLid(lid(i))  ||
                 redirect.flush)
    isBr(i)  := (isBr(i ) || 
                 in ( inId0).isBr  && UIntToOH(in ( inId0).pid)(i) || 
                 in ( inId1).isBr  && UIntToOH(in ( inId1).pid)(i)) && !(
                 out(outId0).valid && UIntToOH(out(outId0).pid)(i) || 
                 out(outId1).valid && UIntToOH(out(outId1).pid)(i) ||
                 redirect.brRBK && redirect.flushedBid(bid(i))  ||
                 redirect.ldRBK && redirect.flushedLid(lid(i))  ||
                 redirect.flush)
    isLd(i)  := (isLd(i ) || 
                 in ( inId0).isLd  && UIntToOH(in ( inId0).pid)(i) || 
                 in ( inId1).isLd  && UIntToOH(in ( inId1).pid)(i)) && !(
                 out(outId0).valid && UIntToOH(out(outId0).pid)(i) || 
                 out(outId1).valid && UIntToOH(out(outId1).pid)(i) ||
                 redirect.brRBK && redirect.flushedBid(bid(i))  ||
                 redirect.ldRBK && redirect.flushedLid(lid(i))  ||
                 redirect.flush)
  } 
   

  for (i <- 0 until NumEntries) {
    val inId0 = i/NumEntriesIn
    val inId1 = PoolInNum - 1 - i/NumEntriesIn
    when (in(inId0).pid === i) {
      iid(i)  := in(inId0).iid
      bid(i) := in(inId0).bid
      lid(i) := in(inId0).lid
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
   
  //state update
  val inSrc1WakeupVec = Wire(Vec(PoolInNum, Vec(UInt(WakeNum.W))))
  val inSrc2WakeupVec = Wire(Vec(PoolInNum, Vec(UInt(WakeNum.W))))
  val inSrc1Wakeup = Wire(Vec(PoolInNum, Bool()))
  val inSrc2Wakeup = Wire(Vec(PoolInNum, Bool()))
  for (i <- 0 until PoolInNum) {
    inSrc1WakeupVec(i) := (0 until WakeNum).map(wakeIn(_).valid && wakeIn(_).dstAddr === in(i).src1Addr && in(i).src1Vld && in(i).valid).asUInt
    inSrc2WakeupVec(i) := (0 until WakeNum).map(wakeIn(_).valid && wakeIn(_).dstAddr === in(i).src2Addr && in(i).src2Vld && in(i).valid).asUInt
    inSrc1Wakeup(i) := inSrc1WakeupVec.orR
    inSrc2Wakeup(i) := inSrc2WakeupVec.orR
  }

  for (i <- 0 until NumEntries) {
    val inId0 = i / inumEntriesIn
    val inId1 = PoolInNum - 1 - i/NumEntriesIn
    val outId0 = i / NumEntriesOut * 2
    val outId1 = i / NumEntriesOut * 2 + 1
    val entrySrc1WakeupVec = Wire(UInt(WakeNum.W))
    val entrySrc1Wakeup    = Wire(Bool())
    entrySrc1WakeupVec := (0 until WakeNum).map(wakeIn(_).valid && wakeIn(_).dstAddr === src1Addr(i) && !src1Rdy(i) && valid(i)).asUInt
    entrySrc1Wakeup := entrySrc1WakeupVec.orR
    src1Rdy(i)  := (src1Rdy(i) || 
                    entrySrc1Wakeup || 
                    in ( inId0).src1Rdy  && UIntToOH(in ( inId0).pid)(i) || 
                    inSrc1Wakeup(inId0)  && UIntToOH(in ( inId0).pid)(i) || 
                    in ( inId1).src1Rdy  && UIntToOH(in ( inId1).pid)(i)) && !(
                    inSrc1Wakeup(inId1)  && UIntToOH(in ( inId1).pid)(i) || 
                    out(outId0).valid    && UIntToOH(out(outId0).pid)(i) || 
                    out(outId1).valid    && UIntToOH(out(outId1).pid)(i) ||
                    redirect.valid && redirect.flushedIid(iid(i)) )
    src1DRdy(i) := (src1DRdy(i) ||
                    entrySrc1Wakeup ||
                    inSrc1Wakeup(inId0)  && UIntToOH(in ( inId0).pid)(i) ||
                    inSrc1Wakeup(inId1)  && UIntToOH(in ( inId1).pid)(i)) && !(
                    in ( inId0).src1Vld  && UIntToOH(in ( inId0).pid)(i) ||
                    in ( inId1).src1Vld  && UIntToOH(in ( inId1).pid)(i) ||
                    out(outId0).valid    && UIntToOH(out(outId0).pid)(i) ||
                    out(outId1).valid    && UIntToOH(out(outId1).pid)(i) ||
                    redirect.valid && redirect.flushedIid(iid(i)) )
    src1FwdSrc_0(i) := entrySrc1WakeupVec ||
                      Mux( UIntToOH(in ( inId0).pid)(i) && in ( inId0).valid, inSrc1WakeupVec(inId0), 0.U)
                      Mux( UIntToOH(in ( inId1).pid)(i) && in ( inId1).valid, inSrc1WakeupVec(inId1), 0.U)
    src1FwdVld_0(i) := entrySrc1Wakeup ||
                      inSrc1Wakeup(inId0)  && UIntToOH(in ( inId0).pid)(i) ||
                      inSrc1Wakeup(inId1)  && UIntToOH(in ( inId1).pid)(i)  
    src1FwdVld_1(i) := src1FwdVld0(i)
    src1FwdSrc_1(i) := src1FwdSrc0(i)
    val entrySrc2WakeupVec = Wire(UInt(WakeNum.W))
    val entrySrc2Wakeup = Wire(Bool())
    entrySrc2WakeupVec := (0 until WakeNum).map(wakeIn(_).valid && wakeIn(_).dstAddr === src2Addr(i) && !src2Rdy(i)).asUInt
    entrySrc2Wakeup := entrySrc2WakeupVec.orR
    src2Rdy(i)  := (src2Rdy(i) || 
                    entrySrc2Wakeup || 
                    in ( inId0).src2Rdy  && UIntToOH(in ( inId0).pid)(i) || 
                    inSrc2Wakeup(inId0)  && UIntToOH(in ( inId0).pid)(i) || 
                    in ( inId1).src2Rdy  && UIntToOH(in ( inId1).pid)(i)) && !(
                    inSrc2Wakeup(inId1)  && UIntToOH(in ( inId1).pid)(i) || 
                    out(outId0).valid  && UIntToOH(out(outId0).pid)(i) || 
                    out(outId1).valid  && UIntToOH(out(outId1).pid)(i) ||
                    redirect.valid && redirect.flushedIid(iid(i)) )
    src2DRdy(i) := (src2DRdy(i) ||
                    entrySrc2Wakeup ||
                    inSrc2Wakeup(inId0)  && UIntToOH(in ( inId0).pid)(i) ||
                    inSrc2Wakeup(inId1)  && UIntToOH(in ( inId1).pid)(i)) && !(
                    in ( inId0).src2Vld  && UIntToOH(in ( inId0).pid)(i) ||
                    in ( inId1).src2Vld  && UIntToOH(in ( inId1).pid)(i) ||
                    out(outId0).valid  && UIntToOH(out(outId0).pid)(i) ||
                    out(outId1).valid  && UIntToOH(out(outId1).pid)(i) ||
                    redirect.valid && redirect.flushedIid(iid(i)) )
    src2FwdSrc_0(i) := entrySrc2WakeupVec ||
                      Mux( UIntToOH(in ( inId0).pid)(i) && in ( inId0).valid, inSrc2WakeupVec(inId0), 0.U)
                      Mux( UIntToOH(in ( inId1).pid)(i) && in ( inId1).valid, inSrc2WakeupVec(inId1), 0.U)
    src2FwdVld_0(i) := entrySrc2Wakeup ||
                      inSrc2Wakeup(inId0)  && UIntToOH(in ( inId0).pid)(i) ||
                      inSrc2Wakeup(inId1)  && UIntToOH(in ( inId1).pid)(i)  
    src2FwdVld_1(i) := src2FwdVld0(i)
    src2FwdSrc_1(i) := src2FwdSrc0(i)
  }
  
  val regReqVld = RegInit(0.U(RFReadNum.W)) 
  val regReqPid = Reg(Vec(RFReadNum, UInt(PidWidth.W)))
  val regReqAddr = Wire(Vec(RFReadNum, UInt(PRegWidth.W)))
  val r0ReqVld = src1Rdy(NumEntries/2-1, 0)  & ~src1DRdy(NumEntries/2-1, 0)
  val r1ReqVld = src2Rdy(NumEntries/2-1, 0)  & ~src2DRdy(NumEntries/2-1, 0)
  val r2ReqVld = src2Rdy(NumEntries-1, NumEntries/2)  & ~src2DRdy(NumEntries-1, NumEntries/2)
  val r3ReqVld = src2Rdy(NumEntries-1, NumEntries/2)  & ~src2DRdy(NumEntries-1, NumEntries/2)
  regReqVld     := Cat(r3ReqVld.orR, r2ReqVld.orR, r1ReqVld.orR, r0ReqVld.orR)
  regReqPid (0) := PriorityEncoder(r0ReqVld)
  regReqAddr(0) := PriorityMux    (r0ReqVld, src1Addr(NumEntries/2-1, 0))
  regReqPid (1) := PriorityEncoder(r1ReqVld)
  regReqAddr(0) := PriorityMux    (r1ReqVld, src2Addr(NumEntries/2-1, 0))
  regReqPid (2) := PriorityEncoder(r2ReqVld) + NumEntries/2
  regReqAddr(2) := PriorityMux    (r2ReqVld, src1Addr(NumEntries-1, NumEntries/2))
  regReqPid (3) := PriorityEncoder(r3ReqVld) + NumEntries/2
  regReqAddr(3) := PriorityMux    (r3ReqVld, src2Addr(NumEntries-1, NumEntries/2))
  
  for (i <- 0 until NumEntries) {
    if(i < NumEntries/2) {
      when(src1FwdVld_1(i)) {
        src1Data(i) := PriorityMux(src1FwdSrc_1, dataIn)
      }.elsewhen(regReqVld(0) && regReqPid(0) === i) {
        src1Data(i) := regRsp(0)
      }
      when(src2FwdVld_1(i)) {
        src2Data(i) := PriorityMux(src2FwdSrc_1, dataIn)
      }.elsewhen(regReqVld(1) && regReqPid(1) === i) {
        src2Data(i) := regRsp(1)
      }
    } else {
      when(src1FwdVld_1(i)) {
        src1Data(i) := PriorityMux(src1FwdSrc_1, dataIn)
      }.elsewhen(regReqVld(2) && regReqPid(2) === i) {
        src1Data(i) := regRsp(2)
      }
      when(src2FwdVld_1(i)) {
        src2Data(i) := PriorityMux(src2FwdSrc_1, dataIn)
      }.elsewhen(regReqVld(3) && regReqPid(3) === i) {
        src2Data(i) := regRsp(3)
      }
    }
  }

  val issueVld      = RegInit(Vec(PoolOutNum, false.B))
  val issuePid      = Reg    (Vec(PoolOutNum, UInt(PidWidth.W)))
  val issueIid      = Wire   (Vec(PoolOutNum, UInt(IidWidth.W)))
  val issueDstVld   = Wire   (Vec(PoolOutNum, Bool())) 
  val issueDstAddr  = Wire   (Vec(PoolOutNum, UInt(PRegWidth.W)))
  val issueSrc1Data = Wire   (Vec(PoolOutNum, UInt(64.W))) 
  val issueSrc2Data = Wire   (Vec(PoolOutNum, UInt(64.W))) 

  for(i <- 0 until PoolOutNum) {
    val MsbNum = NumEntriesOut*(i/2+1)-1
    val LsbNum = NumEntriesOut*(i/2)
    val issueRdy = Wire(UInt((NumEntriesOut*2).W))
    issueRdy := valid(MsbNum, LsbNum) & src1DRdy(MsbNum, LsbNum) & src2DRdy(MsbNum, LsbNum) & (LsbNum until MsbNum).map(dstAddr(_)(0)===i%2) 
    issueVld(i) := issueRdy.orR 
    issuePid(i) := PriorityEncoder(issueRdy) + i/2*NumEntriesOut
    issueDstAddr(i) := dstAddr(issuePid(i)) 
    issueSrc1Data(i) := Mux(src1FwdVld_1(issuePid(i)), PriorityMux(src1FwdSrc_1(issuePid(i)), dataIn), src1Data(issuePid(i)) ) 
    issueSrc2Data(i) := Mux(src2FwdVld_1(issuePid(i)), PriorityMux(src2FwdSrc_1(issuePid(i)), dataIn), src2Data(issuePid(i)) ) 

    out(i).valid := issueVld(i)
    out(i).pid := issuePid(i)
    out(i).dstAddr := issueDstAddr(i)
    out(i).src1Data := issueSrc1Data(i)
    out(i).src2Data :=  issueSrc2Data(i) 
  }

  io.out := RegNext(out)
  io.wakeOut := out
































}
