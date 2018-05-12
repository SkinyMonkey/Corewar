module UnpackInstructionSpec where

import Test.Hspec

import Op
import Vm.UnpackInstruction

testUnpackInstructions =
  describe "UnpackInstruction" $ do
  -- FIXME : clean up test
     it "test getInstructionSize" $ do
      let hasIndex = True
          noIndex  = False
          hasOpCode = True
          noOpCode = False

      getInstructionSize [Register 0, Indirect 0, Direct 0] hasIndex hasOpCode `shouldBe` 7
      getInstructionSize [Register 0, Direct 0, Register 0] noIndex hasOpCode `shouldBe` 8
      getInstructionSize [Direct 0] False noOpCode `shouldBe` 5

-- FIXME : move to UnpackInstructions
--    it "validOpCode" $ do
--      let opCode1 = 255 -- 0b11111111
--      validOpCode opCode1 `shouldBe` True
--
--      let opCode2 = 0 -- 0b00000000
--      validOpCode opCode2 `shouldBe` False
--
--      let opCode3 = 128 -- 0b10000000
--      validOpCode opCode3 `shouldBe` True
--
--      let opCode4 = 204 -- 0b11001100
--      validOpCode opCode4 `shouldBe` False
--
--      let opCode5 = 236 -- 0b11101100
--      validOpCode opCode5 `shouldBe` True
