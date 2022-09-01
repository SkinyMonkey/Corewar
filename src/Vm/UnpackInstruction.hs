{-# LANGUAGE BinaryLiterals #-}

module Vm.UnpackInstruction where

import Data.Word
import Data.Bits
import Data.Binary
import Data.Binary.Get

import Op
import Vm.Vm
import Vm.Instructions
import Vm.CheckInstruction

import Utils

data Instruction = Instruction {
  index :: Int,
  params :: [Parameter],
  instructionSize :: Int,
  cycles :: Int
} deriving (Show, Eq)

registerMask = 0b01000000 :: Word8
directMask   = 0b10000000 :: Word8
indirectMask = 0b11000000 :: Word8
currentMask  = 0b11000000

-- getParameter based on current OpCode
getParameter :: Bool -> Word8 -> Get (Either Parameter ())
getParameter hasIndex opCode
  | currentParameter == registerMask = Left <$> (Register <$> getWord8)
  | currentParameter == indirectMask = Left <$> (Indirect <$> getWord16be)
  | currentParameter == directMask   = Left <$> (Direct   <$> (if hasIndex then fromIntegral <$> getWord16be else getWord32be))
  | otherwise = Right <$> skip 1
  where currentParameter = opCode .&. currentMask

-- FIXME : replace by mapM_
--         zip opCode << 2 * paramIndex?
parseByOpCode :: Bool -> Word8 -> Get (Word8, [Parameter])
parseByOpCode hasIndex opCode = do
  empty <- isEmpty
  if empty || opCode == 0
    then return (opCode, [])
    else do result <- getParameter hasIndex opCode
            (_, parameters) <- parseByOpCode hasIndex (shiftL opCode 2)
            return $ case result of
              Left parameter -> (opCode, parameter:parameters)
              Right _ -> (opCode, parameters)

getParameters hasOpCode hasIndex =
  if hasOpCode 
  then getWord8 >>= parseByOpCode hasIndex
  else parseByOpCode hasIndex directMask -- We know it should be a direct

getInstruction :: Int -> Get (Maybe Instruction)
getInstruction championsNbr = do
  instruction <- getWord8
  if instructionIsValid instruction
  then do 
       let hasIndex =  instruction `elem` haveIndexInstructions
           hasOpCode = instruction `notElem` noOpCodeInstructions
       (opCode, params) <- getParameters hasOpCode hasIndex -- (opCode, params, error)

       let validOpCode = if hasOpCode then opCodeIsValid opCode else True
           validParameters = checkParamsAgainstLegalParamSet instruction championsNbr params

       if validOpCode && validParameters
       then do
            let size = getInstructionSize params hasIndex hasOpCode
                cycles' = nbrCycles (opsbyCode !! (fromIntegral instruction - 1))
                index = fromIntegral instruction - 1
            return $ Just $ Instruction index params size cycles'

       else return Nothing
  else return Nothing
