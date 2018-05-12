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

-- FIXME : check params against Op table?
--         check championNbr number in case of a live instruction
--
--         -- FIXME : list of list, zip (param, legalParameters) together
--         legalParameters = opTable !! instruction
--
--         zip params legalParameters
--
--         isValid Bool -> (Parameter, [ArgType()]) -> Bool
--         isvalid acc (el, legalParameters) =
--          case el of
--            PRegister registerNbr -> register `elem` legalParameters
--                                  && validRegister registerNbr
--                                  && acc
--            PDirect value -> direct `elem` legalParameters
--                          && if instruction == alive
--                             then value > 0 && value < fromIntegral championNbr < length (programs vm)
--                             else True
--                          && acc
--            PIndirect value -> indirect `elem` legalParameters && acc
--
validParams =
  foldl isValid True
  where
    validRegister registerNbr =
      let registerNbr' = fromIntegral registerNbr
      in registerNbr' > 0 && registerNbr' < regNumber

    isValid :: Bool -> Parameter -> Bool
    isValid acc el =
      case el of 
      Register registerNbr -> acc && validRegister registerNbr
      _ -> acc && True

bitmasks = [ 3 `shiftL` x | x <- [0,2,4,6] ]

-- Apply bitmasks from right to left
-- As soon as a valid value (>0) is met, a flag is set
-- Any value == 0 after that will make the opCode invalid
validOpCode :: Word8 -> Bool
validOpCode opCode =
  fst $ foldl (valid opCode) (False, False) bitmasks
  where
    valid :: Word8 ->  (Bool, Bool) -> Word8 -> (Bool, Bool)
    valid opCode (False, False) mask = if (opCode .&. mask) > 0 then (True, True) else (False, False)
    valid opCode (True, True)   mask = (opCode .&. mask > 0, True)
    valid opCode (False, True)  _    = (False, True)

getInstruction :: Get (Maybe Instruction)
getInstruction = do
  instruction <- getWord8
  if validInstruction instruction
  then do 
       let hasIndex =  instruction `elem` haveIndexInstructions
           hasOpCode = instruction `notElem` noOpCodeInstructions
       (opCode, params) <- getParameters hasOpCode hasIndex

       if validOpCode opCode && validParams params
       then do
            let size = getInstructionSize params hasIndex hasOpCode
                cycles' = nbrCycles (opsbyCode !! (fromIntegral instruction - 1))
                index = fromIntegral instruction - 1
            return $ Just $ Instruction index params size cycles'

       else return Nothing
  else return Nothing -- error $ "BAD INSTRUCTION : " ++ show instruction -- Nothing
