object Main extends App {
    new (chisel3.stage.ChiselStage).emitVerilog(
      new BitVecReg(100),
      Array("--emission-options=disableMemRandomization,disableRegisterRandomization")
    )
    new (chisel3.stage.ChiselStage).emitVerilog(
      new BitVecReg1(100),
      Array("--emission-options=disableMemRandomization,disableRegisterRandomization")
    )
    new (chisel3.stage.ChiselStage).emitVerilog(
      new RegVecReg(32, 64),
      Array("--emission-options=disableMemRandomization,disableRegisterRandomization")
    )
    new (chisel3.stage.ChiselStage).emitVerilog(
      new RegVecReg1(32, 64),
      Array("--emission-options=disableMemRandomization,disableRegisterRandomization")
    )
}

