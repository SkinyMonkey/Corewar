{-# LANGUAGE BinaryLiterals #-}

module UnpackInstructionSpec where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Maybe
import Data.Word

import Test.Hspec

import Op
import Vm.Vm
import Vm.UnpackInstruction
import Vm.Instructions

packTestInstruction :: String -> Word8 -> [Word8] -> BL.ByteString
packTestInstruction mnemonic opCode paramBytes =
    let opCodeBytes = if opCode > 0 then [opCode] else []
    in BL.pack $ [instructionByMnemonic mnemonic] ++ opCodeBytes ++ paramBytes ++ replicate 32 0

testUnpackInstructions =
  describe "UnpackInstruction" $ do
    let championsNbr = 4
        lengthWithOpCode = 2
        lengthWithoutOpCode = 1

    it "getInstruction with a valid instruction using a direct without index"$ do
      let paramBytes = [0, 0, 0, 1]
          memory = packTestInstruction "live" 0 paramBytes

          index = 0
          params = [Direct 1]
          size = length paramBytes + lengthWithoutOpCode
          cycles = 10
          expectedResult = Just $ Instruction index params size cycles

          getInstruction' = getInstruction championsNbr
          instruction = runGet getInstruction' memory 

      instruction `shouldBe` expectedResult

    it "getInstruction with a valid instruction using a register"$ do
      let opCode = 0b10010000
          paramBytes = [0, 0, 0, 1] ++ [1]
          memory = packTestInstruction "ld" opCode paramBytes

          index = 1
          params = [Direct 1, Register 1]
          size = length paramBytes + lengthWithOpCode
          cycles = 5
          expectedResult = Just $ Instruction index params size cycles

          getInstruction' = getInstruction championsNbr
          instruction = runGet getInstruction' memory 

      instruction `shouldBe` expectedResult

    it "getInstruction with a valid instruction using a direct with an index"$ do
      let paramBytes = [ 0, 1 ]
          -- 9 == zjmp?
          memory = packTestInstruction "zjmp" 0 paramBytes

          index = 8
          params = [Direct 1]
          size = length paramBytes + lengthWithoutOpCode
          cycles = 20
          expectedResult = Just $ Instruction index params size cycles

          getInstruction' = getInstruction championsNbr
          instruction = runGet getInstruction' memory 

      instruction `shouldBe` expectedResult

    it "getInstruction with a valid instruction using a direct with an index in second position"$ do
      let opCode = 0b01100100
          paramBytes = [1] ++ [ 0, 1 ] ++ [1]

          memory = packTestInstruction "ldi" opCode paramBytes

          index = 9
          params = [Register 1, Direct 1, Register 1]
          size = length paramBytes + lengthWithOpCode
          cycles = 25
          expectedResult = Just $ Instruction index params size cycles

          getInstruction' = getInstruction championsNbr
          instruction = runGet getInstruction' memory 

      instruction `shouldBe` expectedResult

    it "getInstruction with an invalid instruction"$ do
      let memory = BL.pack $ replicate memSize 0
          getInstruction' = getInstruction championsNbr
          instruction = runGet getInstruction' memory

      instruction `shouldBe` Nothing

    it "getInstruction with an invalid op code"$ do
      let memory = BL.pack $ [2, 1]
          getInstruction' = getInstruction championsNbr
          instruction = runGet getInstruction' memory

      instruction `shouldBe` Nothing

    it "getInstruction with an invalid params"$ do
      let memory = BL.pack $ [1, 0, 0, 0, 42]
          getInstruction' = getInstruction championsNbr
          instruction = runGet getInstruction' memory

      instruction `shouldBe` Nothing
