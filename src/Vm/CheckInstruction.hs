{-# LANGUAGE BinaryLiterals #-}

module Vm.CheckInstruction where

import Data.Word
import Data.Bits

import Op
import Vm.Vm
import Vm.Instructions

import Utils

getInstructionSize :: [Parameter] -> Bool -> Bool -> Int
getInstructionSize params hasIndex hasOpCode =
  let accumulatedSize = if hasOpCode then 2 else 1
  in foldl compute accumulatedSize params
  where
    compute :: Int -> Parameter -> Int
    compute accumulatedSize el = case el of
      Register _ -> accumulatedSize + 1 -- 1 byte, sizeOf word8
      Indirect _ -> accumulatedSize + 2 -- 2 bytes, sizeOf word16
      Direct   _ -> accumulatedSize + (if hasIndex then 2 else 4) -- ..

instructionIsValid instruction = instruction > 0 && fromIntegral instruction < length instructionTable

registerIsValid registerNbr = registerNbr > 0 && fromIntegral registerNbr < regNumber

checkParamAgainstLegalParamSet :: InstructionCode -> Int -> Bool -> (Parameter, [Parameter]) -> Bool
checkParamAgainstLegalParamSet instruction championsNbr previousParamIsValid (el, legalParameters) =
   previousParamIsValid &&
   case el of
   Register registerNbr -> register `elem` legalParameters && registerIsValid registerNbr
   Indirect value -> indirect `elem` legalParameters
   Direct value -> direct `elem` legalParameters
                && if instruction == 0x01 -- alive
                   then value > 0 && fromIntegral value < championsNbr
                   else True

-- one param against the corresponding index in the parameterSet
checkParamsAgainstLegalParamSet :: InstructionCode -> Int -> [Parameter] -> Bool
checkParamsAgainstLegalParamSet instruction championsNbr params =
  let op = opsbyCode !! (fromIntegral instruction - 1)
      legalParameters = argsType op
      legalArgNbr = nbrArgs op
      checkParamAgainstLegalParamSet' = checkParamAgainstLegalParamSet instruction championsNbr

  in (length params == legalArgNbr) &&
    (foldl checkParamAgainstLegalParamSet' True $ zip params legalParameters)

-- opCodeIsValid : Apply bitmasks from right to left
-- As soon as a valid value (>0) is met, a flag is set
-- Any value == 0 after that will make the opCode invalid

-- We use foldl so the bitmasks go from right to left:
bitmasks = [
 0b00000011,
 0b00001100,
 0b00110000,
 0b11000000]

opCodeIsValid :: OpCode -> Bool
opCodeIsValid opCode =
  fst $ foldl (valid opCode) (False, False) bitmasks
  where
    --  (Bool, Bool) = (validOpcode, validValueFlag)
    valid :: Word8 ->  (Bool, Bool) -> Word8 -> (Bool, Bool)
    valid opCode (False, False) mask = if (opCode .&. mask) > 0
                                       then (True, True)
                                       else (False, False)
    valid opCode (True, True)   mask = (opCode .&. mask > 0, True)
    valid opCode (False, True)  _    = (False, True)
