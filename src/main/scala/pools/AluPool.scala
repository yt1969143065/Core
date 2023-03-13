package pools

import chisel3._
import chisel3.util._

import tools._


//alu + load + branch
class AluPool extends Module with CoreParameters{
  val io = IO(new Bundle {
    val redirect = Input(new Redirect) 
    val poolFull = Output(Bool()) 
    val poolId = Output(Vec(PoolInNum, UInt(PidWidth.W)))
    val in = Input(Vec(PoolInNum, new MicroOp()))  
    val regReq = Output(Vec(RFReadNum, UInt(PRegWidth.W)))
    val regRsp = Input(Vec(RFReadNum, UInt(64.W))) 
    val out = Output(Vec(PoolOutNum, new ExeBundle())) 
    val wakeOut = Output(Vec(PoolOutNum, new WakeupBundle())) 
    val wakeIn = Input(Vec(WakeNum, new WakeupBundle()))
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
  val valid        = RegInit(VecInit(Seq.tabulate(NumEntries)(i => false.B)))
  val isAl         = RegInit(VecInit(Seq.tabulate(NumEntries)(i => false.B)))
  val isLd         = RegInit(VecInit(Seq.tabulate(NumEntries)(i => false.B)))
  val isBr         = RegInit(VecInit(Seq.tabulate(NumEntries)(i => false.B)))
  val iid          = Reg    (Vec    (NumEntries, UInt(IidWidth.W)))
  val lid          = Reg    (Vec    (NumEntries, UInt(LidWidth.W)))
  val bid          = Reg    (Vec    (NumEntries, UInt(BidWidth.W)))
  val src1Addr     = Reg    (Vec    (NumEntries, UInt(PRegWidth.W)))
  val src2Addr     = Reg    (Vec    (NumEntries, UInt(PRegWidth.W)))
  val dstVld       = RegInit(VecInit(Seq.tabulate(NumEntries)(i => false.B)))
  val dstAddr      = Reg    (Vec    (NumEntries, UInt(PRegWidth.W)))
  
  val src1Rdy      = RegInit(VecInit(Seq.tabulate(NumEntries)(i => false.B)))
  val src1DRdy     = RegInit(VecInit(Seq.tabulate(NumEntries)(i => false.B)))
  val src1FwdVld_0 = RegInit(VecInit(Seq.tabulate(NumEntries)(i => false.B)))
  val src1FwdVld_1 = RegInit(VecInit(Seq.tabulate(NumEntries)(i => false.B)))
  val src1FwdSrc_0 = RegInit(VecInit(Seq.tabulate(NumEntries)(i => 0.U(WakeNum.W))))
  val src1FwdSrc_1 = RegInit(VecInit(Seq.tabulate(NumEntries)(i => 0.U(WakeNum.W))))
  val src1Data     = Reg    (Vec    (NumEntries, UInt(64.W)))
  
  val src2Rdy      = RegInit(VecInit(Seq.tabulate(NumEntries)(i => false.B)))
  val src2DRdy     = RegInit(VecInit(Seq.tabulate(NumEntries)(i => false.B)))
  val src2FwdVld_0 = RegInit(VecInit(Seq.tabulate(NumEntries)(i => false.B)))
  val src2FwdVld_1 = RegInit(VecInit(Seq.tabulate(NumEntries)(i => false.B)))
  val src2FwdSrc_0 = RegInit(VecInit(Seq.tabulate(NumEntries)(i => 0.U(WakeNum.W))))
  val src2FwdSrc_1 = RegInit(VecInit(Seq.tabulate(NumEntries)(i => 0.U(WakeNum.W))))
  val src2Data     = Reg    (Vec    (NumEntries, UInt(64.W)))

  val poolFull     = RegInit(false.B)
  val inPid        = RegInit(VecInit((0 until PoolInNum).map(i => if(i%2 == 1) ((i/2+1)*NumEntriesIn-1).asUInt else (i/2*NumEntriesIn).asUInt)))
  //wire
  val out      = Wire(Vec(PoolOutNum, new IssueBundle))
  val validNxt = Wire(Vec(NumEntries, Bool()))
  

  poolFull := (0 until PoolInNum/2).map( i => 
              PopCount(validNxt.asUInt(( i +1)*NumEntriesIn-1, i * NumEntriesIn)) > (NumEntriesIn-4).asUInt
              ).reduce(_ | _) 

  for(i <- 0 until PoolInNum/2) {
    inPid(  i) := PriorityEncoder(~valid.asUInt((i+1)*NumEntriesIn-1, i*NumEntriesIn)) + (i*NumEntriesIn).asUInt
    inPid(7-i) := (NumEntriesIn - 1).asUInt - PriorityEncoder(Reverse(~valid.asUInt((i+1)*NumEntriesIn-1, i*NumEntriesIn))) + (i*NumEntriesIn).asUInt
  }

  //valid update
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
    valid(i) := validNxt(i)
  } 
   

  for (i <- 0 until NumEntries) {
    val inId0 = i/NumEntriesIn
    val inId1 = PoolInNum - 1 - i/NumEntriesIn
    when ( in(inId0).valid && in(inId0).pid === i.asUInt) {
      iid(i)       := in(inId0).iid
      bid(i)       := in(inId0).bid
      lid(i)       := in(inId0).lid
      src1Addr(i)  := in(inId0).src1Addr
      src2Addr(i)  := in(inId0).src2Addr
      dstAddr(i)   := in(inId0).src2Addr
      dstVld(i)    := in(inId0).dstVld
      isAl(i)      := in(inId0).isAl
      isLd(i)      := in(inId0).isLd
      isBr(i)      := in(inId0).isBr
      
    }.elsewhen( in(inId1).valid && in(inId1).pid === i.asUInt) {
      iid(i)       := in(inId1).iid
      bid(i)       := in(inId1).bid
      lid(i)       := in(inId1).lid
      src1Addr(i)  := in(inId1).src1Addr
      src2Addr(i)  := in(inId1).src2Addr
      dstAddr(i)   := in(inId1).dstAddr
      dstVld(i)    := in(inId1).dstVld
      isAl(i)      := in(inId1).isAl
      isLd(i)      := in(inId1).isLd
      isBr(i)      := in(inId1).isBr
    }
  } 
   
  //state update
  val inSrc1WakeupVec = Wire(Vec(PoolInNum, UInt(WakeNum.W)))
  val inSrc2WakeupVec = Wire(Vec(PoolInNum, UInt(WakeNum.W)))
  val inSrc1Wakeup = Wire(Vec(PoolInNum, Bool()))
  val inSrc2Wakeup = Wire(Vec(PoolInNum, Bool()))
  for (i <- 0 until PoolInNum) {
    inSrc1WakeupVec(i) := VecInit((0 until WakeNum).map(j => wakeIn(j).valid && wakeIn(j).dstAddr === in(i).src1Addr && in(i).src1Vld && in(i).valid)).asUInt
    inSrc2WakeupVec(i) := VecInit((0 until WakeNum).map(j => wakeIn(j).valid && wakeIn(j).dstAddr === in(i).src2Addr && in(i).src2Vld && in(i).valid)).asUInt
    inSrc1Wakeup(i) := inSrc1WakeupVec(i).orR
    inSrc2Wakeup(i) := inSrc2WakeupVec(i).orR
  }

  for (i <- 0 until NumEntries) {
    val inId0 = i / NumEntriesIn
    val inId1 = PoolInNum - 1 - i/NumEntriesIn
    val outId0 = i / NumEntriesOut * 2
    val outId1 = i / NumEntriesOut * 2 + 1
    val entrySrc1WakeupVec = Wire(UInt(WakeNum.W))
    val entrySrc1Wakeup    = Wire(Bool())
    entrySrc1WakeupVec := VecInit((0 until WakeNum).map(j => wakeIn(j).valid && wakeIn(j).dstAddr === src1Addr(i) && !src1Rdy(i) && valid(i))).asUInt
    entrySrc1Wakeup := entrySrc1WakeupVec.orR
    src1Rdy(i)  := (src1Rdy(i) || 
                    entrySrc1Wakeup || 
                    in ( inId0).src1Rdy  && UIntToOH(in ( inId0).pid)(i) || 
                    inSrc1Wakeup(inId0)  && UIntToOH(in ( inId0).pid)(i) || 
                    in ( inId1).src1Rdy  && UIntToOH(in ( inId1).pid)(i)) && !(
                    inSrc1Wakeup(inId1)  && UIntToOH(in ( inId1).pid)(i) || 
                    out(outId0).valid    && UIntToOH(out(outId0).pid)(i) || 
                    out(outId1).valid    && UIntToOH(out(outId1).pid)(i) ||
                    redirect.brRBK && redirect.flushedBid(bid(i))  ||
                    redirect.ldRBK && redirect.flushedLid(lid(i))  ||
                    redirect.flush)
    src1DRdy(i) := (src1DRdy(i) ||
                    entrySrc1Wakeup ||
                    inSrc1Wakeup(inId0)  && UIntToOH(in ( inId0).pid)(i) ||
                    inSrc1Wakeup(inId1)  && UIntToOH(in ( inId1).pid)(i)) && !(
                    in ( inId0).src1Vld  && UIntToOH(in ( inId0).pid)(i) ||
                    in ( inId1).src1Vld  && UIntToOH(in ( inId1).pid)(i) ||
                    out(outId0).valid    && UIntToOH(out(outId0).pid)(i) ||
                    out(outId1).valid    && UIntToOH(out(outId1).pid)(i) ||
                    redirect.brRBK && redirect.flushedBid(bid(i))  ||
                    redirect.ldRBK && redirect.flushedLid(lid(i))  ||
                    redirect.flush)
    src1FwdSrc_0(i) := entrySrc1WakeupVec |
                      Mux( UIntToOH(in ( inId0).pid)(i) && in ( inId0).valid, inSrc1WakeupVec(inId0), 0.U)
                      Mux( UIntToOH(in ( inId1).pid)(i) && in ( inId1).valid, inSrc1WakeupVec(inId1), 0.U)
    src1FwdVld_0(i) := entrySrc1Wakeup ||
                      inSrc1Wakeup(inId0)  && UIntToOH(in ( inId0).pid)(i) ||
                      inSrc1Wakeup(inId1)  && UIntToOH(in ( inId1).pid)(i)  
    src1FwdVld_1(i) := src1FwdVld_0(i)
    src1FwdSrc_1(i) := src1FwdSrc_0(i)
    val entrySrc2WakeupVec = Wire(UInt(WakeNum.W))
    val entrySrc2Wakeup = Wire(Bool())
    entrySrc2WakeupVec := VecInit((0 until WakeNum).map(j => wakeIn(j).valid && wakeIn(j).dstAddr === src2Addr(i) && !src2Rdy(i))).asUInt
    entrySrc2Wakeup := entrySrc2WakeupVec.orR
    src2Rdy(i)  := (src2Rdy(i) || 
                    entrySrc2Wakeup || 
                    in ( inId0).src2Rdy  && UIntToOH(in ( inId0).pid)(i) || 
                    inSrc2Wakeup(inId0)  && UIntToOH(in ( inId0).pid)(i) || 
                    in ( inId1).src2Rdy  && UIntToOH(in ( inId1).pid)(i)) && !(
                    inSrc2Wakeup(inId1)  && UIntToOH(in ( inId1).pid)(i) || 
                    out(outId0).valid  && UIntToOH(out(outId0).pid)(i) || 
                    out(outId1).valid  && UIntToOH(out(outId1).pid)(i) ||
                    redirect.brRBK && redirect.flushedBid(bid(i))  ||
                    redirect.ldRBK && redirect.flushedLid(lid(i))  ||
                    redirect.flush)
    src2DRdy(i) := (src2DRdy(i) ||
                    entrySrc2Wakeup ||
                    inSrc2Wakeup(inId0)  && UIntToOH(in ( inId0).pid)(i) ||
                    inSrc2Wakeup(inId1)  && UIntToOH(in ( inId1).pid)(i)) && !(
                    in ( inId0).src2Vld  && UIntToOH(in ( inId0).pid)(i) ||
                    in ( inId1).src2Vld  && UIntToOH(in ( inId1).pid)(i) ||
                    out(outId0).valid  && UIntToOH(out(outId0).pid)(i) ||
                    out(outId1).valid  && UIntToOH(out(outId1).pid)(i) ||
                    redirect.brRBK && redirect.flushedBid(bid(i))  ||
                    redirect.ldRBK && redirect.flushedLid(lid(i))  ||
                    redirect.flush)
    src2FwdSrc_0(i) := entrySrc2WakeupVec |
                      Mux( UIntToOH(in ( inId0).pid)(i) && in ( inId0).valid, inSrc2WakeupVec(inId0), 0.U)
                      Mux( UIntToOH(in ( inId1).pid)(i) && in ( inId1).valid, inSrc2WakeupVec(inId1), 0.U)
    src2FwdVld_0(i) := entrySrc2Wakeup ||
                      inSrc2Wakeup(inId0)  && UIntToOH(in ( inId0).pid)(i) ||
                      inSrc2Wakeup(inId1)  && UIntToOH(in ( inId1).pid)(i)  
    src2FwdVld_1(i) := src2FwdVld_0(i)
    src2FwdSrc_1(i) := src2FwdSrc_0(i)
  }
  
  val regReqVld = RegInit(0.U(RFReadNum.W)) 
  val regReqPid = Reg(Vec(RFReadNum, UInt(PidWidth.W)))
  val regReqAddr = Wire(Vec(RFReadNum, UInt(PRegWidth.W)))
  val r0ReqVld = src1Rdy.asUInt(NumEntries/2-1, 0)           & ~src1DRdy.asUInt(NumEntries/2-1, 0)
  val r1ReqVld = src2Rdy.asUInt(NumEntries/2-1, 0)           & ~src2DRdy.asUInt(NumEntries/2-1, 0)
  val r2ReqVld = src2Rdy.asUInt(NumEntries-1, NumEntries/2)  & ~src2DRdy.asUInt(NumEntries-1, NumEntries/2)
  val r3ReqVld = src2Rdy.asUInt(NumEntries-1, NumEntries/2)  & ~src2DRdy.asUInt(NumEntries-1, NumEntries/2)
  regReqVld     := Cat(r3ReqVld.orR, r2ReqVld.orR, r1ReqVld.orR, r0ReqVld.orR)
  regReqPid (0) := PriorityEncoder(r0ReqVld)
  regReqAddr(0) := PriorityMux    (r0ReqVld, src1Addr.slice(0, NumEntries/2).reverse)
  regReqPid (1) := PriorityEncoder(r1ReqVld)
  regReqAddr(1) := PriorityMux    (r1ReqVld, src2Addr.slice(0, NumEntries/2).reverse)
  regReqPid (2) := PriorityEncoder(r2ReqVld) + (NumEntries/2).asUInt
  regReqAddr(2) := PriorityMux    (r2ReqVld, src1Addr.slice(NumEntries/2, NumEntries).reverse)
  regReqPid (3) := PriorityEncoder(r3ReqVld) + (NumEntries/2).asUInt
  regReqAddr(3) := PriorityMux    (r3ReqVld, src2Addr.slice(NumEntries/2, NumEntries).reverse)

  for (i <- 0 until NumEntries) {
    if(i < NumEntries/2) {
      when(src1FwdVld_1(i)) {
        src1Data(i) := Mux1H(src1FwdSrc_1(i), dataIn)
      }.elsewhen(regReqVld(0) && regReqPid(0) === i.asUInt) {
        src1Data(i) := regRsp(0)
      }
      when(src2FwdVld_1(i)) {
        src2Data(i) := Mux1H(src2FwdSrc_1(i), dataIn)
      }.elsewhen(regReqVld(1) && regReqPid(1) === i.asUInt) {
        src2Data(i) := regRsp(1)
      }
    } else {
      when(src1FwdVld_1(i)) {
        src1Data(i) := Mux1H(src1FwdSrc_1(i), dataIn)
      }.elsewhen(regReqVld(2) && regReqPid(2) === i.asUInt) {
        src1Data(i) := regRsp(2)
      }
      when(src2FwdVld_1(i)) {
        src2Data(i) := Mux1H(src2FwdSrc_1(i), dataIn)
      }.elsewhen(regReqVld(3) && regReqPid(3) === i.asUInt) {
        src2Data(i) := regRsp(3)
      }
    }
  }

  val selectVld = Wire(Vec(PoolOutNum, Bool()))
  val selectPid = Wire(Vec(PoolOutNum, UInt(PidWidth.W)))
  val selectBid = Wire(Vec(PoolOutNum, UInt(BidWidth.W)))
  val selectLid = Wire(Vec(PoolOutNum, UInt(LidWidth.W)))

  val issueVld      = RegInit(VecInit(Seq.tabulate(PoolOutNum)(i => false.B)))
  val issuePid      = Reg    (Vec    (PoolOutNum, UInt(PidWidth.W)))
  val issueBid      = Reg    (Vec    (PoolOutNum, UInt(BidWidth.W)))
  val issueLid      = Reg    (Vec    (PoolOutNum, UInt(LidWidth.W)))
  val issueIid      = Wire   (Vec    (PoolOutNum, UInt(IidWidth.W)))
  val issueDstAddr  = Wire   (Vec    (PoolOutNum, UInt(PRegWidth.W)))
  val issueSrc1Data = Wire   (Vec    (PoolOutNum, UInt(64.W))) 
  val issueSrc2Data = Wire   (Vec    (PoolOutNum, UInt(64.W))) 

  for(i <- 0 until PoolOutNum) {
    val MsbNum = NumEntriesOut*(i/2+1)-1
    val LsbNum = NumEntriesOut*(i/2)
    val selectRdy = Wire(UInt((NumEntriesOut*2).W))
    selectRdy := valid.asUInt(MsbNum, LsbNum) & dstVld.asUInt(MsbNum, LsbNum) & src1DRdy.asUInt(MsbNum, LsbNum) & src2DRdy.asUInt(MsbNum, LsbNum) & VecInit((LsbNum until MsbNum).map(dstAddr(_)(0)=== (i%2).asUInt)).asUInt 
    selectVld(i) := selectRdy.orR 
    selectPid(i) := PriorityEncoder(selectRdy) + (i/2*NumEntriesOut).asUInt
    selectLid(i) := lid(issuePid(i)) 
    selectBid(i) := bid(issuePid(i)) 

    issueVld(i)      := Mux(redirect.flush ||  redirect.brRBK && redirect.flushedBid(selectBid(i)) || redirect.ldRBK && redirect.flushedLid(selectLid(i)), false.B, selectVld(i))
    issuePid(i)      := selectPid(i)
    issueLid(i)      := selectLid(i)
    issueBid(i)      := selectBid(i)
    issueDstAddr(i)  := dstAddr(issuePid(i)) 
    issueSrc1Data(i) := Mux(src1FwdVld_1(issuePid(i)), PriorityMux(src1FwdSrc_1(issuePid(i)), dataIn), src1Data(issuePid(i)) ) 
    issueSrc2Data(i) := Mux(src2FwdVld_1(issuePid(i)), PriorityMux(src2FwdSrc_1(issuePid(i)), dataIn), src2Data(issuePid(i)) ) 
    issueIid(i)      := iid(issuePid(i)) 

    out(i) := DontCare
    out(i).valid := Mux(redirect.flush ||  redirect.brRBK && redirect.flushedBid(issueBid(i)) || redirect.ldRBK && redirect.flushedLid(issueLid(i)), false.B, issueVld(i))
    out(i).pid := issuePid(i)
    out(i).dstAddr := issueDstAddr(i)
    out(i).src1Data := issueSrc1Data(i)
    out(i).src2Data :=  issueSrc2Data(i) 
  }
  io.wakeOut := out
  io.out := RegNext(out)
  io.poolFull := poolFull
  io.poolId := inPid
  io.regReq := regReqAddr







}
