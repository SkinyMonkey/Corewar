module UnpackInstructionSpec where

import Test.Hspec

import Vm.Vm
import Vm.UnpackInstruction

testUnpackInstructions =
  describe "UnpackInstruction" $ do
     it "test getInstructionSize" $ do
      let hasIndex = True
          noIndex  = False
          hasOpCode = True
          noOpCode = False

      getInstructionSize [PRegister 0, PIndirect 0, PDirect 0] hasIndex hasOpCode `shouldBe` 7
      getInstructionSize [PRegister 0, PDirect 0, PRegister 0] noIndex hasOpCode `shouldBe` 8
      getInstructionSize [PDirect 0] False noOpCode `shouldBe` 5
