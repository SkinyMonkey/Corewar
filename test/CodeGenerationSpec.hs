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
        encodeParameter accumulator (Register 1) `shouldBe` (4, 64)

      it "should encodeParameter a direct at index 1" $ do
        encodeParameter accumulator (Direct 1) `shouldBe` (4, 128)

      it "should encodeParameter a indirect at index 1" $ do
        encodeParameter accumulator (Indirect 1) `shouldBe` (4, 192)

    describe "generateOpCode" $ do
--      it "should generate a byte equal to 0x68" $ do
--        let params = [Register 1, Indirect 2, Indirect 3]
--        generateOpCode params `shouldBe` 0x68

      it "should generate a byte equal to 0x78" $ do
        let params = [Register 2, Indirect 23, Direct 34]
        generateOpCode params `shouldBe` 0x78

      it "should generate a byte equal to 0xF8" $ do
        let params = [Indirect 23, Indirect 45, Direct 34]
        generateOpCode params `shouldBe` 0xF8

      it "should generate a byte equal to 0x5C" $ do
        let params = [Register 1, Register 3, Indirect 34]
        generateOpCode params `shouldBe` 0x5C

      it "should generates an opcode for 1 parameter" $ do
        let params = [Register 0]
        generateOpCode params `shouldBe` 64
   
      it "should generates an opcode for 2 parameters" $ do
        let params = [Register 0, Register 0]
        generateOpCode params `shouldBe` 80
        
      it "should generates an opcode for 3 parameters" $ do
        let params = [Register 0, Register 0, Register 0]
        generateOpCode params `shouldBe` 84

      it "should generates an opcode for 4 parameters" $ do
        let params = [Register 0, Register 0, Register 0, Register 0]
        generateOpCode params `shouldBe` 85

      -- FIXME : use quickcheck? but can't know the results
      --         or do it by hand?
      -- it "should work for every combinations" $ do
      -- putStrLn $ show $ generateOpCode' params

    describe "serialize" $ do
      -- TODO real checks
      --      use pack to create ByteString to compare
      it "serializeParameter" $ do
        let instruction = 0x01 -- live
            param = Register 1
            expectedResult = Prelude.map c2w "\1"
        (B.concat $ BL.toChunks $ runPut $ serializeParameter championData instruction param) `shouldBe` B.pack expectedResult

      it "serializeParameters" $ do
        let instruction = 0x01 -- live
            params = [Register 1, Direct 2, Indirect 3]
            expectedResult = map c2w "\1\0\0\0\2\0\3"
        (B.concat $ BL.toChunks $ runPut $ serializeParameters championData instruction params) `shouldBe` B.pack expectedResult

      describe "serializeInstruction" $ do
        it "serialize an instruction without an opcode but with its parameters" $ do
          -- 1 == live, FIXME : get it from op
          let instructionCode = 1
              params = [Register 1, Direct 2, Indirect 3]
              instruction = (instructionCode, params)
              expectedResult = Prelude.map c2w "\1\1\0\0\0\2\0\3"
          (B.concat $ BL.toChunks $ runPut $ serializeInstruction championData instruction) `shouldBe` B.pack expectedResult
   
        it "serialize an instruction with an opcode and its parameters" $ do
          let instructionCode = 2
              params = [Register 1, Direct 2, Indirect 3]
              instruction = (instructionCode, params)
              expectedResult = Prelude.map c2w "\2\108\1\0\0\0\2\0\3"
          (B.concat $ BL.toChunks $ runPut $ serializeInstruction championData instruction) `shouldBe` B.pack expectedResult

        it "serialize an instruction with an opcode and its parameters" $ do
          let instructionCode = 0x0b
              params = [Register 1, Indirect 2, Indirect 3]
              instruction = (instructionCode, params)
              expectedResult = Prelude.map c2w "\11\124\1\0\2\0\3"
          (B.concat $ BL.toChunks $ runPut $ serializeInstruction championData instruction) `shouldBe` B.pack expectedResult

      -- it "serializeInstructions" $ do
      -- it "serializeHeader" $ do
