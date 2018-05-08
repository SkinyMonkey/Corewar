module VmSpec where

import Test.Hspec
import Test.QuickCheck

import Vm.Vm

testVm =
  describe "Vm" $ do
--    let program = newProgram 0 "test" 0 $ pack ""
--        vm = newVm [program]

--    it "w8stoW32 && w32tow8s" $ do
--      w = 1000000000000
--      (w8stoW32 $ w32tow8s w) `shouldBe` w
--
--    it "w8stoW32le && w32tow8sle" $ do
--      w = 100000000000
--      (w8stoW32le $ w32tow8sle w) `shouldBe` w

    it "validOpCode" $ do
      let opCode1 = 255 -- 0b11111111
      validOpCode opCode1 `shouldBe` True

      let opCode2 = 0 -- 0b00000000
      validOpCode opCode2 `shouldBe` False

      let opCode3 = 128 -- 0b10000000
      validOpCode opCode3 `shouldBe` True

      let opCode4 = 204 -- 0b11001100
      validOpCode opCode4 `shouldBe` False

      let opCode5 = 236 -- 0b11101100
      validOpCode opCode5 `shouldBe` True

--    it "getCurrentProgram" $ do
--      getCurrentProgram vm `shouldBe` program
--
--    it "setCurrentProgram" $ do
--      let program' = newProgram 0 "test'" 0 $ pack ""
--          vm' = setCurrentProgram vm program
--      getCurrentProgram vm `shouldBe` program'
--
--    it "setCurrentProgramRegister" $ do
--      let vm' = setCurrentProgramRegister 1 32 vm
--          regs = registers $ getCurrentProgram vm'
--      regs !! 1 `shouldbe` 32
--
--    it "getCurrentProgramRegister" $ do
--      let reg = getCurrentProgramRegister 1 vm
--      reg `shouldBe` 0
--
--    it "getCurrentProgramCarry" $ do
--      let carry = getCurrentProgramCarry vm
--      carry `shouldBe` False
--
--    it "setCurrentProgramCarry" $ do
--      let vm' = setCurrentProgramCarry True vm
--          carry = getCurrentProgramCarry vm'
--      carry `shouldBe` True
--
--    it "getCurrentProgramPc" $ do
--      let pc = getCurrentProgramPc vm
--      pc `shouldBe` 0
--
--    it "setCurrentProgramPc" $ do
--      let vm' = setCurrentProgramPc 32 vm
--          pc = getCurrentProgramPc vm'
--      pc `shouldBe` 32
--
--    it "updateMemory" $ do
--      let xs = pack "\0\0\0\0\0\0\0\0\0\0"
--          vm' = updateMemory xs 4 12345 4
--      memory vm' `shouldBe` pack "\0\0\0\009\0\0\0\0"
--
--    it "setMemoryByCurrentProgramPc" $ do
--      -- FIXME implement
--
--    it "getValueFromMemory" $ do
--      -- FIXME implement
