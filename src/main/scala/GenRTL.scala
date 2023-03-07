/*
import chisel3._
import tools._

object Main extends App {
  new (chisel3.stage.ChiselStage).emitVerilog(
    new BufferOldestSelect(),
    Array("--emission-options=disableMemRandomization,disableRegisterRandomization") 
  )
}
*/

import chisel3._

import tools._
import pools._

object Main extends App {
  new (chisel3.stage.ChiselStage).emitVerilog(
    new AluPool(),
    Array("--emission-options=disableMemRandomization,disableRegisterRandomization") 
  )
}
