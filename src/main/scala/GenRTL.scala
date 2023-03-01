import chisel3._
import tools._

object Main extends App {
  new (chisel3.stage.ChiselStage).emitVerilog(
    new BufferOldestSelect(),
    Array("--emission-options=disableMemRandomization,disableRegisterRandomization") 
  )
}
