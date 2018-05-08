module Vm.UnpackInstruction where

import Data.Word
import Data.Bits
import Data.Binary
import Data.Binary.Get

import Op
import Vm.Vm
import Vm.Instructions

data Instruction = Instruction {
  handler :: ([Parameter] -> Vm -> Maybe Vm),
  params :: [Parameter],
  instructionSize :: Int,
  cycles :: Int
}

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
  let acc = if hasOpCode then 2 else 1
  in foldl compute acc params
  where
    compute :: Int -> Parameter -> Int
    compute acc el = case el of
      PRegister _ -> acc + 1 -- 1 byte, sizeOf word8
      PIndirect _ -> acc + 2 -- 2 bytes, sizeOf word16
      PDirect   _ -> acc + (if hasIndex then 2 else 4) -- ..

getParameters hasOpCode hasIndex =
  if hasOpCode 
  then getWord8 >>= parseByOpCode hasIndex
  else parseByOpCode hasIndex directMask -- We know it should be a direct

-- FIXME : Get (Maybe Instruction)
--         -> check opCode is ok with validOpCode
--         -> check that params are valid
getInstruction :: Get (Maybe Instruction)
getInstruction = do
  instruction <- getWord8
  if instruction > 0 && fromIntegral instruction < (length instructionTable)
  then do 
       let hasIndex =  instruction `elem` haveIndexInstructions
           hasOpCode = not $ instruction `elem` noOpCodeInstructions
       params <- getParameters hasOpCode hasIndex
       let size = getInstructionSize params hasIndex hasOpCode
           cycles' = nbrCycles (opsbyCode !! fromIntegral (instruction - 1))
           handler = instructionTable !! ((fromIntegral instruction) - 1)
       return $ Just $ Instruction handler params size cycles'
  else return $ Nothing -- error $ "BAD INSTRUCTION : " ++ show instruction -- Nothing
