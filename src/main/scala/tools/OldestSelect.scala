package tools

import chisel3._
import chisel3.util._


//class BufferOldestSelect (numEntries: Int = 16, numEnq: Int = 2, numDeq: Int = 2) extends Module {
class BufferOldestSelect (numEntries: Int = 16) extends Module {
  val numEnq = 2
  val numDeq = 2
  val io = IO(new Bundle {
    val enq = Input (Vec(numEnq, UInt(numEntries.W)))
    val deq = Input (Vec(numDeq, UInt(numEntries.W)))
    val req = Input (Vec(numEnq, UInt(numEntries.W))) 
    val gnt = Output(Vec(numDeq, UInt(numEntries.W)))  
  })
  val enq = io.enq
  val deq = io.deq.reduce(_ | _) 
  val req = io.req
  val gnt = io.gnt

  assert(req.reduce(_ & _) === 0.U, "BufferOldestSelect req has bit conflict")

  //ageMatrix update
  val ageMatrix = RegInit(VecInit(Seq.fill(numEntries)(0.U(numEntries.W))))
  for(i <- 0 until numEntries){
    when (enq(1)(i)) {
      ageMatrix(i) := ( ageMatrix.reduce(_ | _) | enq(0) | enq(1) ) & (~deq) 
    }.elsewhen(enq(0)(i)) {
      ageMatrix(i) := ( ageMatrix.reduce(_ | _) | enq(0) ) & (~deq) 
    }.elsewhen(deq.orR) {
      ageMatrix(i) := ageMatrix(i) & (~deq)
    }.otherwise {
      ageMatrix(i) := ageMatrix(i)
    }
  }
  //ageMatrix use
  val ageMatrixMasked = Wire(Vec(2, Vec(numEntries, UInt(numEntries.W))))
  for(i <- 0 until 2) {
    for(j <- 0 until numEntries) {
      ageMatrixMasked(i)(j) := ageMatrix(j) & req(i)
    }
    io.gnt(i) := VecInit((0 until numEntries).map(row => ((UIntToOH(row.asUInt, numEntries)) | ~ageMatrixMasked(i)(row)).andR)).asUInt
   
  }

  
}
