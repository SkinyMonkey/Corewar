module Vm.UnpackInstruction where

import Data.Word
import Data.Bits
import Data.Binary
import Data.Binary.Get

import Op
import Vm.Vm

data Instruction = Instruction {
  instruction :: Word8,
  params :: [Parameter],
  instructionSize :: Int,
  cycles :: Int
} deriving (Show, Eq)

registerMask = shiftL 1 6 :: Word8 -- 0b01000000
directMask   = shiftL 2 6 :: Word8 -- 0b10000000
indirectMask = shiftL 3 6 :: Word8 -- 0b11000000
currentMask  = shiftL 3 6 -- 0b11000000

getParameter :: Bool -> Word8 -> Get Parameter
getParameter hasIndex opCode
  | currentParameter == registerMask = PRegister <$> getWord8
  | currentParameter == indirectMask = PIndirect <$> getWord16be
  | currentParameter == directMask   = PDirect   <$> (if hasIndex then fromIntegral <$> getWord16be else getWord32be)
  -- FIXME : if we try to decode a bad opCode
  --         could be == 0b00000000
  --         what to do then, error will just crash the Vm
  | otherwise = error $ "Unknown parameter type " ++ show currentParameter
  where currentParameter = opCode .&. currentMask

-- FIXME : replace by mapM_
--         zip opCode << 2 * paramIndex?
parseByOpCode :: Bool -> Word8 -> Get [Parameter]
parseByOpCode hasIndex opCode = do
  empty <- isEmpty
  if empty || opCode == 0
    then return []
    else do parameter <- getParameter hasIndex opCode
            parameters <- parseByOpCode hasIndex (shiftL opCode 2) -- safe?
            return (parameter:parameters)

getInstructionSize :: [Parameter] -> Bool -> Bool -> Int
getInstructionSize params hasIndex hasOpCode =
  let size = foldl compute 0 params
  in if hasOpCode
     then 3 + size -- instruction + opcode -- FIXME: why 3, should be 2
     else 2 + size -- instruction -- FIXME : why 2, should be 1
  where
    compute :: Int -> Parameter -> Int
    compute acc el = case el of
      PRegister _ -> acc + 1 -- 1 byte, sizeOf word8
      PIndirect _ -> acc + 2 -- 2 bytes, sizeOf word16
      PDirect   _ -> acc + (if hasIndex then 2 else 4) -- ..

getInstruction :: Get Instruction
getInstruction = do
  instruction <- getWord8
  let hasIndex = (instruction `elem` haveIndexInstructions)
      hasOpCode = instruction `elem` noOpCodeInstructions
  params <- if hasOpCode 
           then parseByOpCode hasIndex directMask -- getWord32be  -- False?
           else getWord8 >>= -- FIXME : is it semantically correct?
                parseByOpCode hasIndex
  let size = getInstructionSize params hasIndex hasOpCode
      cycles' = nbrCycles (opsbyCode !! fromIntegral (instruction - 1))
  return $ Instruction instruction params size cycles'
