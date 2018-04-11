module InstructionsSpec where

import Test.Hspec
import Test.QuickCheck

import Vm.Vm

testVm 
  describe "Vm" $ do
    let program = newProgram 0 "test" 0 $ pack ""
        vm = newVm [program]

    it "getCurrentProgram" $ do
      getCurrentProgram vm `shouldBe` program

    it "setCurrentProgram" $ do
      let program' = newProgram 0 "test'" 0 $ pack ""
          vm' = setCurrentProgram vm program
      getCurrentProgram vm `shouldBe` program'

    it "setCurrentProgramRegister" $ do
      let vm' = setCurrentProgramRegister 1 32 vm
          regs = registers $ getCurrentProgram vm'
      regs !! 1 `shouldbe` 32

    it "getCurrentProgramRegister" $ do
      let reg = getCurrentProgramRegister 1 vm
      reg `shouldBe` 0

    it "getCurrentProgramCarry" $ do
      let carry = getCurrentProgramCarry vm
      carry `shouldBe` False

    it "setCurrentProgramCarry" $ do
      let vm' = setCurrentProgramCarry True vm
          carry = getCurrentProgramCarry vm'
      carry `shouldBe` True

    it "getCurrentProgramPc" $ do
      let pc = getCurrentProgramPc vm
      pc `shouldBe` 0

    it "setCurrentProgramPc" $ do
      let vm' = setCurrentProgramPc 32 vm
          pc = getCurrentProgramPc vm'
      pc `shouldBe` 32

    it "updateMemory" $ do
      let xs = pack "\0\0\0\0\0\0\0\0\0\0"
          vm' = updateMemory xs 4 12345 4
      memory vm' `shouldBe` pack "\0\0\0\009\0\0\0\0"

    it "setMemoryByCurrentProgramPc" $ do
      -- FIXME implement

    it "getValueFromMemory" $ do
      -- FIXME implement
