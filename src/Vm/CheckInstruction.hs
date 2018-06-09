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
  let acc = if hasOpCode then 2 else 1
  in foldl compute acc params
  where
    compute :: Int -> Parameter -> Int
    compute acc el = case el of
      Register _ -> acc + 1 -- 1 byte, sizeOf word8
      Indirect _ -> acc + 2 -- 2 bytes, sizeOf word16
      Direct   _ -> acc + (if hasIndex then 2 else 4) -- ..

instructionIsValid instruction = instruction > 0 && fromIntegral instruction < length instructionTable

registerIsValid registerNbr = registerNbr > 0 && fromIntegral registerNbr < regNumber

-- one param against the corresponding index in the parameterSet
checkParamAgainstLegalParamSet :: InstructionCode -> Int -> Bool -> (Parameter, Parameter) -> Bool
checkParamAgainstLegalParamSet instruction championsNbr previousParamIsValid (el, legalParameter) = case el of
  Register registerNbr -> previousParamIsValid
                         && legalParameter == register
                         && registerIsValid registerNbr
  Indirect value -> previousParamIsValid && legalParameter == indirect
  Direct value -> previousParamIsValid
                 && legalParameter == direct
                 && if instruction == 0x01 -- alive
                    then value > 0 && fromIntegral value < championsNbr
                    else True

-- all the params against one parameterSet
checkParamsAgainstLegalParamSets :: InstructionCode -> Int -> [Parameter] -> Bool -> [Parameter] -> Bool
checkParamsAgainstLegalParamSets instruction championsNbr params foundOneValidSet legalParamSet =
  if length params /= length legalParamSet
  then foundOneValidSet
  else let checkParamAgainstLegalParamSet' = checkParamAgainstLegalParamSet instruction championsNbr
           toValidate = zip params legalParamSet
       in foldl checkParamAgainstLegalParamSet' True toValidate || foundOneValidSet

-- all the params against all the parameterSets
parametersAreValid :: InstructionCode -> Int -> [Parameter] -> Bool
parametersAreValid instruction championsNbr params =
  let legalParamSets = argsType (opsbyCode !! (fromIntegral instruction - 1))
      checkParamsAgainstLegalParamSets' = checkParamsAgainstLegalParamSets instruction championsNbr params
  in foldl checkParamsAgainstLegalParamSets' False legalParamSets

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
    valid opCode (False, False) mask = if (opCode .&. mask) > 0 then (True, True) else (False, False)
    valid opCode (True, True)   mask = (opCode .&. mask > 0, True)
    valid opCode (False, True)  _    = (False, True)
