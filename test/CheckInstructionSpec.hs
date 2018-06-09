{-# LANGUAGE BinaryLiterals #-}

module CheckInstructionSpec where

import Data.List
import Data.Word

import Test.Hspec
import Test.HUnit.Base (Test(TestCase, TestList, TestLabel), assertEqual, assertBool)
import Test.HUnit.Text (runTestTT)

import Op
import Vm.CheckInstruction
import Vm.Instructions

import Debug.Trace

-- FIXME : how to generate these without recoding opCodeIsValid
inopCodeIsValids = [0b00000000,0b00000001,0b00000010,0b00000011,0b00000100,0b00000101,0b00000110,0b00000111,0b00001000,0b00001001,0b00001010,0b00001011,0b00001100,0b00001101,0b00001110,0b00001111,0b00010000,0b00010001,0b00010010,0b00010011,0b00010100,0b00010101,0b00010110,0b00010111,0b00011000,0b00011001,0b00011010,0b00011011,0b00011100,0b00011101,0b00011110,0b00011111,0b00100000,0b00100001,0b00100010,0b00100011,0b00100100,0b00100101,0b00100110,0b00100111,0b00101000,0b00101001,0b00101010,0b00101011,0b00101100,0b00101101,0b00101110,0b00101111,0b00110000,0b00110001,0b00110010,0b00110011,0b00110100,0b00110101,0b00110110,0b00110111,0b00111000,0b00111001,0b00111010,0b00111011,0b00111100,0b00111101,0b00111110,0b00111111,0b01000001,0b01000010,0b01000011,0b01000100,0b01000101,0b01000110,0b01000111,0b01001000,0b01001001,0b01001010,0b01001011,0b01001100,0b01001101,0b01001110,0b01001111,0b01010001,0b01010010,0b01010011,0b01100001,0b01100010,0b01100011,0b01110001,0b01110010,0b01110011,0b10000001,0b10000010,0b10000011,0b10000100,0b10000101,0b10000110,0b10000111,0b10001000,0b10001001,0b10001010,0b10001011,0b10001100,0b10001101,0b10001110,0b10001111,0b10010001,0b10010010,0b10010011,0b10100001,0b10100010,0b10100011,0b10110001,0b10110010,0b10110011,0b11000001,0b11000010,0b11000011,0b11000100,0b11000101,0b11000110,0b11000111,0b11001000,0b11001001,0b11001010,0b11001011,0b11001100,0b11001101,0b11001110,0b11001111,0b11010001,0b11010010,0b11010011,0b11100001,0b11100010,0b11100011,0b11110001,0b11110010,0b11110011]

opCodeIsValids = [0b01000000,0b01010000,0b01010100,0b01010101,0b01010110,0b01010111,0b01011000,0b01011001,0b01011010,0b01011011,0b01011100,0b01011101,0b01011110,0b01011111,0b01100000,0b01100100,0b01100101,0b01100110,0b01100111,0b01101000,0b01101001,0b01101010,0b01101011,0b01101100,0b01101101,0b01101110,0b01101111,0b01110000,0b01110100,0b01110101,0b01110110,0b01110111,0b01111000,0b01111001,0b01111010,0b01111011,0b01111100,0b01111101,0b01111110,0b01111111,0b10000000,0b10010000,0b10010100,0b10010101,0b10010110,0b10010111,0b10011000,0b10011001,0b10011010,0b10011011,0b10011100,0b10011101,0b10011110,0b10011111,0b10100000,0b10100100,0b10100101,0b10100110,0b10100111,0b10101000,0b10101001,0b10101010,0b10101011,0b10101100,0b10101101,0b10101110,0b10101111,0b10110000,0b10110100,0b10110101,0b10110110,0b10110111,0b10111000,0b10111001,0b10111010,0b10111011,0b10111100,0b10111101,0b10111110,0b10111111,0b11000000,0b11010000,0b11010100,0b11010101,0b11010110,0b11010111,0b11011000,0b11011001,0b11011010,0b11011011,0b11011100,0b11011101,0b11011110,0b11011111,0b11100000,0b11100100,0b11100101,0b11100110,0b11100111,0b11101000,0b11101001,0b11101010,0b11101011,0b11101100,0b11101101,0b11101110,0b11101111,0b11110000,0b11110100,0b11110101,0b11110110,0b11110111,0b11111000,0b11111001,0b11111010,0b11111011,0b11111100,0b11111101,0b11111110,0b11111111]

parameterFrom :: Parameter -> Parameter
parameterFrom parameter | parameter == register = Register 1
                        | parameter == indirect = Indirect 1
                        | parameter == direct = Direct 1

wrongParameterFrom :: Parameter -> Parameter
wrongParameterFrom parameter | parameter == register = Direct 1
                        | parameter == indirect = Register 1
                        | parameter == direct = Indirect 1

generateValidParameters legalParameters = 
   [ [ parameterFrom legalParam | legalParam <- legalParameter ] | legalParameter <- legalParameters]

generateInvalidParameters legalParameters = 
   [ [ wrongParameterFrom legalParam | legalParam <- legalParameter ] | legalParameter <- legalParameters]

parameterName (Register v) = "register " ++ show v
parameterName (Indirect v) = "indirect " ++ show v
parameterName (Direct v) = "direct " ++ show v

validParameterTestCase instruction params = 
         let instructionName = opsNames !! (fromIntegral instruction - 1)
             parameters = intercalate " " $ map parameterName params
             caseName = "valid case : " ++ instructionName ++ " " ++ parameters
         in TestCase $ assertBool caseName $ parametersAreValid instruction 4 params

invalidParameterTestCase instruction params = 
         let instructionName = opsNames !! (fromIntegral instruction - 1)
             parameters = intercalate " " $ map parameterName params
             caseName = "invalid case : " ++ instructionName ++ " " ++ parameters
         in TestCase $ assertBool caseName $ not $ parametersAreValid instruction 4 params

testCheckInstructions =
  describe "CheckInstruction" $ do
    it "getInstructionSize" $ do
      let hasIndex = True
          noIndex  = False
          hasOpCode = True
          noOpCode = False

      getInstructionSize [Register 0, Indirect 0, Direct 0] hasIndex hasOpCode `shouldBe` 7
      getInstructionSize [Register 0, Direct 0, Register 0] noIndex hasOpCode `shouldBe` 8
      getInstructionSize [Direct 0] False noOpCode `shouldBe` 5

    it "opCodeIsValid" $ do
      mapM_ (\opCode -> opCodeIsValid opCode `shouldBe` True) opCodeIsValids
      mapM_ (\opCode -> opCodeIsValid opCode `shouldBe` False) inopCodeIsValids

    it "instructionIsValid" $ do
       instructionIsValid 0 `shouldBe` False
       instructionIsValid (length instructionTable) `shouldBe` False

       mapM_ (\instruction -> instructionIsValid instruction `shouldBe` True) [1..length instructionTable - 1]

    it "registerIsValid" $ do
       registerIsValid 0 `shouldBe` False
       registerIsValid regNumber `shouldBe` False

       mapM_ (\register -> registerIsValid register `shouldBe` True) [1..regNumber - 1]

    it "parametersAreValid" $ do
      let instructions = [1..fromIntegral $ length instructionTable - 1::Word8]

      let validParams = map (\op -> generateValidParameters (argsType op) ) opsbyCode
      let instructionsWithValidParams = (zip instructions validParams)

      let paramToTest = \(instruction, paramCases) ->
               map (validParameterTestCase instruction) paramCases

      let tests = concat $ map paramToTest instructionsWithValidParams

      -- for each op, create invalid combinations of parameters
      let invalidParams = map (\op -> generateInvalidParameters (argsType op) ) opsbyCode
      let instructionsWithInvalidParams = (zip instructions invalidParams)

      let invalidParamToTest = \(instruction, paramCases) ->
               map (invalidParameterTestCase instruction) paramCases

      let testInvalidCases = concat $ map invalidParamToTest instructionsWithInvalidParams

      runTestTT $ TestList $ tests ++ testInvalidCases

-- DEBUG : in case a test fail, check it by hand like this:
--      let live = instructionByMnemonic "live"
--      let ld = instructionByMnemonic "ld"
--      let st = instructionByMnemonic "st"
--      let lld = instructionByMnemonic "lld"
--
--      parametersAreValid live 4 [Direct 1] `shouldBe` True
--      parametersAreValid ld 4 [Direct 1, Indirect 1] `shouldBe` True
--      parametersAreValid ld 4 [Register 1] `shouldBe` True
--      parametersAreValid live 4 [Direct 1, Indirect 1] `shouldBe` False
--      parametersAreValid ld 4 [Direct 1] `shouldBe` False
--      parametersAreValid st 4 [Register 1, Direct 1] `shouldBe` False
--      parametersAreValid lld 4 [Direct 1] `shouldBe` False

      return ()
