{-# LANGUAGE BinaryLiterals #-}

module UnpackInstructionSpec where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Maybe

import Test.Hspec

import Op
import Vm.Vm
import Vm.UnpackInstruction
import Vm.Instructions

testUnpackInstructions =
  describe "UnpackInstruction" $ do
    it "getInstruction with a valid instruction using a direct without index"$ do
      let paramBytes = [0, 0, 0, 0b00000001] -- 4 * 8 bits
          memory = BL.pack $ [1] ++ paramBytes ++ replicate 32 0
          getInstruction' = getInstruction 4
          instruction = runGet getInstruction' memory 
          index = 0
          params = [Direct 1]
          size = 5
          cycles = 10
          expectedResult = Just $ Instruction index params size cycles

      instruction `shouldBe` expectedResult

    it "getInstruction with a valid instruction using a register"$ do
      let opCode = [0b01000000] -- 8 bits
          paramBytes = [1] -- 8 bits
          memory = BL.pack $ [2] ++ opCode ++ paramBytes ++ replicate 32 0
          getInstruction' = getInstruction 4
          instruction = runGet getInstruction' memory 
          index = 1
          params = [Register 1]
          size = 3
          cycles = 5
          expectedResult = Just $ Instruction index params size cycles

      instruction `shouldBe` expectedResult

    it "getInstruction with a valid instruction using a direct without index and an indirect"$ do
      let opCode = [0b10110000] -- 8 bits
          paramBytes = [0, 0, 0, 1]  ++ [ 0, 1]
          memory = BL.pack $ [2] ++ opCode ++ paramBytes ++ replicate 32 0
          getInstruction' = getInstruction 4
          instruction = runGet getInstruction' memory 
          index = 1
          params = [Direct 1, Indirect 1]
          size = 8
          cycles = 5
          expectedResult = Just $ Instruction index params size cycles

      instruction `shouldBe` expectedResult

    it "getInstruction with a valid instruction using a direct with an index"$ do
      let paramBytes = [ 0, 1 ]
          memory = BL.pack $ [9] ++ paramBytes ++ replicate 32 0
          getInstruction' = getInstruction 4
          instruction = runGet getInstruction' memory 
          index = 8
          params = [Direct 1]
          size = 3
          cycles = 20
          expectedResult = Just $ Instruction index params size cycles

      instruction `shouldBe` expectedResult

    it "getInstruction with a valid instruction using a direct with an index in second position"$ do
      let opCode = [0b01100000]
          paramBytes = [1] ++ [ 0, 1 ]
          memory = BL.pack $ [10] ++ opCode ++ paramBytes ++ replicate 32 0
          getInstruction' = getInstruction 4
          instruction = runGet getInstruction' memory 
          index = 9
          params = [Register 1, Direct 1]
          size = 5
          cycles = 25
          expectedResult = Just $ Instruction index params size cycles

      instruction `shouldBe` expectedResult

    it "getInstruction with an invalid instruction"$ do
      let memory = BL.pack $ replicate memSize 0
          getInstruction' = getInstruction 4
          instruction = runGet getInstruction' memory

      instruction `shouldBe` Nothing

    it "getInstruction with an invalid op code"$ do
      let memory = BL.pack $ [2, 1]
          getInstruction' = getInstruction 4
          instruction = runGet getInstruction' memory

      instruction `shouldBe` Nothing

    it "getInstruction with an invalid params"$ do
      let memory = BL.pack $ [1, 0, 0, 0, 42]
          getInstruction' = getInstruction 4
          instruction = runGet getInstruction' memory

      instruction `shouldBe` Nothing
