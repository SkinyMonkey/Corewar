module Vm.UnpackInstruction where

import Data.Word
import Data.Bits
import Data.Binary
import Data.Binary.Get

import Op
import Vm.Vm
import Vm.Instructions

data Instruction = Instruction {
  index :: Int,
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
  | currentParameter == registerMask = Register <$> getWord8
  | currentParameter == indirectMask = Indirect <$> getWord16be
  | currentParameter == directMask   = Direct   <$> (if hasIndex then fromIntegral <$> getWord16be else getWord32be)
  -- FIXME : if we try to decode a bad opCode
  --         could be == 0b00000000
  --         what to do then, error will just crash the Vm
  | otherwise = error $ "Unknown parameter type " ++ show currentParameter
  where currentParameter = opCode .&. currentMask

-- FIXME : replace by mapM_
--         zip opCode << 2 * paramIndex?
parseByOpCode :: Bool -> Word8 -> Get (Word8, [Parameter])
parseByOpCode hasIndex opCode = do
  empty <- isEmpty
  if empty || opCode == 0
    then return (opCode, [])
    else do parameter <- getParameter hasIndex opCode
            (_, parameters) <- parseByOpCode hasIndex (shiftL opCode 2) -- safe?
            return (opCode, parameter:parameters)

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

getParameters hasOpCode hasIndex =
  if hasOpCode 
  then getWord8 >>= parseByOpCode hasIndex
  else parseByOpCode hasIndex directMask -- We know it should be a direct

validInstruction instruction = instruction > 0 && fromIntegral instruction < length instructionTable
validRegister registerNbr =
  let registerNbr' = fromIntegral registerNbr
  in registerNbr' > 0 && registerNbr' < regNumber

validParameter :: InstructionCode -> Int -> Bool -> (Parameter, [Parameter]) -> Bool
validParameter instruction championsNbr acc (el, legalParameters) = case el of
   Register registerNbr -> register `elem` legalParameters
                         && validRegister registerNbr
                         && acc
   Indirect value -> indirect `elem` legalParameters && acc
   Direct value -> direct `elem` legalParameters
                 && if instruction == 0x01 -- alive
                    then value > 0 && fromIntegral value < championsNbr
                    else True
                 && acc

validParameters :: InstructionCode -> Int -> [Parameter] -> Bool
validParameters instruction championsNbr params =
  let legalParameters = argsType (opsbyCode !! (fromIntegral instruction - 1))
  in foldl (validParameter instruction championsNbr) True $ zip params legalParameters

-- validOpCode : Apply bitmasks from right to left
-- As soon as a valid value (>0) is met, a flag is set
-- Any value == 0 after that will make the opCode invalid

bitmasks = [ 3 `shiftL` x | x <- [0,2,4,6] ]

validOpCode :: OpCode -> Bool
validOpCode opCode =
  fst $ foldl (valid opCode) (False, False) bitmasks
  where
    valid :: Word8 ->  (Bool, Bool) -> Word8 -> (Bool, Bool)
    valid opCode (False, False) mask = if (opCode .&. mask) > 0 then (True, True) else (False, False)
    valid opCode (True, True)   mask = (opCode .&. mask > 0, True)
    valid opCode (False, True)  _    = (False, True)

getInstruction :: Int -> Get (Maybe Instruction)
getInstruction championsNbr = do
  instruction <- getWord8
  if validInstruction instruction
  then do 
       let hasIndex =  instruction `elem` haveIndexInstructions
           hasOpCode = instruction `notElem` noOpCodeInstructions
       (opCode, params) <- getParameters hasOpCode hasIndex

       if validOpCode opCode && validParameters instruction championsNbr params
       then do
            let size = getInstructionSize params hasIndex hasOpCode
                cycles' = nbrCycles (opsbyCode !! (fromIntegral instruction - 1))
                index = fromIntegral instruction - 1
            return $ Just $ Instruction index params size cycles'

       else return Nothing
  else return Nothing -- 
