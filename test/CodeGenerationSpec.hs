{-# LANGUAGE BinaryLiterals #-}

module CodeGenerationSpec where

import Test.Hspec
import Control.Exception (evaluate)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import Data.Word
import Data.Binary.Put

import Op
import Asm.ChampionData
import Asm.Generation.CodeGeneration

testCodeGeneration =
  describe "CodeGeneration" $ do
    let championData = newChampionData "test"

    describe "encodeParameter" $ do
      let index = 6
          emptyByte = 0
          accumulator = (index, emptyByte)
      it "should encodeParameter a register at index 1" $ do
        encodeParameter accumulator (Register 1) `shouldBe` (4, 0b01000000)

      it "should encodeParameter a direct at index 1" $ do
        encodeParameter accumulator (Direct 1) `shouldBe` (4, 0b10000000)

      it "should encodeParameter a indirect at index 1" $ do
        encodeParameter accumulator (Indirect 1) `shouldBe` (4, 0b11000000)

    describe "generateOpCode" $ do
--      it "should generate a byte equal to 0x68" $ do
--        let params = [Register 1, Indirect 2, Indirect 3]
--        generateOpCode params `shouldBe` 0x68
      it "should generate a byte equal to 104 " $ do
        let params = [Register 1,Direct 12,Direct 42]
        generateOpCode params `shouldBe` 0b01101000

      it "should generate a byte equal to 0b01101100" $ do
        let params = [Register 2, Direct 34, Indirect 32]
        generateOpCode params `shouldBe` 0b01101100

      it "should generate a byte equal to 0b01111000" $ do
        let params = [Register 2, Indirect 23, Direct 34]
        generateOpCode params `shouldBe` 0b01111000

      it "should generate a byte equal to 0b11111000" $ do
        let params = [Indirect 23, Indirect 45, Direct 34]
        generateOpCode params `shouldBe` 0b11111000

      it "should generate a byte equal to 0b01011100" $ do
        let params = [Register 1, Register 3, Indirect 34]
        generateOpCode params `shouldBe` 0b01011100

      it "should generates an opcode for 1 parameter" $ do
        let params = [Register 0]
        generateOpCode params `shouldBe` 0b01000000
   
      it "should generates an opcode for 2 parameters" $ do
        let params = [Register 0, Register 0]
        generateOpCode params `shouldBe` 0b01010000
        
      it "should generates an opcode for 3 parameters" $ do
        let params = [Register 0, Register 0, Register 0]
        generateOpCode params `shouldBe` 0b01010100

      it "should generates an opcode for 4 parameters" $ do
        let params = [Register 0, Register 0, Register 0, Register 0]
        generateOpCode params `shouldBe` 0b01010101

    let runFullPut p = B.concat . BL.toChunks . runPut 

    describe "serialize" $ do
      -- TODO real checks
      --      use pack to create ByteString to compare
      it "serializeParameter" $ do
        let instruction = 0x01 -- live
            param = Register 1
            expectedBytes = Prelude.map c2w "\1"
        (B.concat $ BL.toChunks $ runPut  $ serializeParameter championData instruction param) `shouldBe` B.pack expectedBytes

      it "serializeParameters" $ do
        let instruction = instructionByMnemonic "live"
            params = [Register 1, Direct 2, Indirect 3]
            expectedBytes = map c2w "\1\0\0\0\2\0\3"
        (B.concat $ BL.toChunks $ runPut $ serializeParameters championData instruction params) `shouldBe` B.pack expectedBytes

      describe "serializeInstruction" $ do
        it "serialize an instruction without an opcode but with its parameters" $ do
          let instructionCode = instructionByMnemonic "live"
              params = [Direct 2]
              instruction = (instructionCode, params)
              expectedBytes = [instructionCode] ++ [0, 0, 0, 2]
          (B.concat $ BL.toChunks $ runPut $ serializeInstruction championData instruction) `shouldBe` B.pack expectedBytes
   
        it "serialize an instruction with an opcode and its parameters" $ do
          let instructionCode = instructionByMnemonic "ld"
              params = [Indirect 3, Register 1]
              instruction = (instructionCode, params)
              opCode = 0b11010000
              expectedBytes = [instructionCode, opCode] ++ [0, 3] ++ [1]
          (B.concat $ BL.toChunks $ runPut $ serializeInstruction championData instruction) `shouldBe` B.pack expectedBytes

        it "serialize an instruction with an index, an opcode and its parameters" $ do
          let instructionCode = instructionByMnemonic "sti"
              params = [Register 1, Indirect 2, Direct 3]
              opCode = 0b01111000
              instruction = (instructionCode, params)
              expectedBytes = [instructionCode, opCode] ++ [1] ++ [0, 2] ++ [0, 3]
          (B.concat $ BL.toChunks $ runPut $ serializeInstruction championData instruction) `shouldBe` B.pack expectedBytes

      -- it "serializeInstructions" $ do
      -- it "serializeHeader" $ dot
