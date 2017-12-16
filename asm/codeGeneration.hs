module CodeGeneration (
  generateCode,
) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import Data.Binary.Put

import Op
import Header
import ChampionData
import Data.Word
import Data.Bits

-- FIXME : DEBUG
import Debug.Trace

serializeRegister register | trace ("serializeRegister :" ++ (show register)) False = undefined
serializeRegister register = do
 putWord32be register

serializeDirect direct | trace ("serializeDirect :" ++ (show direct)) False = undefined
serializeDirect direct = do
  putWord32be direct

serializeIndirect indirect | trace ("serializeIndirect :" ++ (show indirect)) False = undefined
serializeIndirect indirect = do
  putWord16be indirect

-- FIXME : get cd, code 
serializeLabel :: Word32 -> Put

serializeLabel labelOffset | trace ("serializeLabel :" ++ (show labelOffset)) False = undefined

serializeLabel labelOffset = do
-- serializeIndirect labelOffset?
  putWord32be labelOffset

-- FIXME : add a function to opCode's binary value
serializeOpCode opCode | trace ("serializeOpCode :" ++ (show opCode)) False = undefined
serializeOpCode opCode = do
 putWord8 opCode

serializeInstructionCode instructionCode | trace ("serializeInstructionCode :" ++ opStr) False = undefined
--  where opStr = (opsNames !! (fromIntegral(instructionCode) ::Int))
    where opStr = show instructionCode
serializeInstructionCode instructionCode = do
  putWord8 instructionCode

-- 01 register
-- 10 direct
-- 11 indirect
encode parameterType index
  | parameterType == register = shiftL 0x01 index
  | parameterType == direct = shiftL 0x02 index
  | parameterType == indirect = shiftL 0x03 index
  | otherwise = error "Unknown parameter type : this should never happen."

-- FIXME : compiler says pattern matche(s) are non-exhaustive
-- but if we add it stops on a parse error ...
-- generateOpCode' (_, _) : ((_, _) : ((_, _) : ((_, _) : (_ : _)))) =
--  error "Too much parameters : this should never happen."

generateOpCode' ((t1, _):(t2, _):(t3, _):(t4, _):[]) =
  encode t1 6 .|. (encode t2 4 .|. (encode t3 2 .|. encode t4 0))
generateOpCode' ((t1, _):(t2, _):(t3, _):[]) =
  encode t1 6 .|. (encode t2 4 .|. encode t3 2)
generateOpCode' ((t1, _):(t2, _):[]) =
  encode t1 6 .|. encode t2 4
generateOpCode' ((t1, _):[]) = 
  encode t1 6

generateOpCode' [] = error "No parameters : this should never happen."

generateOpCode instruction =
  generateOpCode' parameters
  where parameters = snd instruction

generateOffset cd label = fromIntegral $ offset * (-1)
  where offset = (getByteCount cd) - (getLabelOffset cd label)

generateParameter cd parameter
  | parameterType == register = serializeRegister $ (read parameterValue :: Word32)
  | parameterType == direct = serializeDirect $ (read parameterValue :: Word32)
  | parameterType == indirect = serializeIndirect $ (read parameterValue :: Word16)
  | parameterType == label = serializeLabel $ (generateOffset cd parameterValue :: Word32)
  | otherwise = error "Unknown parameter type : this should never happen."
  where parameterType = fst parameter
        parameterValue = snd parameter

-- How to apply a monad on each parameters and chain result?
serializeParameters _ [] = error "No arguments given."

serializeParameters cd [parameter] = do 
  generateParameter cd parameter

serializeParameters cd (headParameter:tailParameters) = do
  generateParameter cd headParameter
  serializeParameters cd tailParameters

serializeInstructionWithOpCode cd instructionCode opCode parameters = do
  serializeInstructionCode instructionCode
  serializeOpCode opCode
  serializeParameters cd parameters

serializeInstructionWithoutOpCode cd instructionCode parameters = do
  serializeInstructionCode instructionCode
  serializeParameters cd parameters

-- FIXME : find a way to branch instead of using multiple similare functions
serializeInstruction cd instruction = do
  if not $ instructionCode `elem` noOpCodeInstructions
  then serializeInstructionWithOpCode cd instructionCode opCode parameters
  else serializeInstructionWithoutOpCode cd instructionCode parameters
  where instructionCode = fst instruction
        opCode = (generateOpCode instruction) :: Word8
        parameters = snd instruction

writeInstruction cd instruction = do
  B.appendFile fileName
    $ B.concat
    $ BL.toChunks
    $ runPut
    $ serializeInstruction cd instruction
  return $ incCounter cd opCode args
  where fileName = getFileName cd
        opCode = fst instruction
        args = snd instruction

writeInstructions' cd [] = do return cd
writeInstructions' cd [instruction] = do
  res <- writeInstruction cd instruction
  return res

writeInstructions' cd (headInstruction:tailInstruction) = do
  updatedCd <- writeInstruction cd headInstruction
  res <- writeInstructions' updatedCd tailInstruction
  return res

-- FIXME : HERE : pass cd along from this function
writeInstructions cd = do
  res <- writeInstructions' cd instructions
  return res
  where instructions = getInstructions cd

finished fileName = putStrLn $ "Generation complete for " ++ fileName
--finished (False, cd) = putStrLn $ "Generation failed for " ++ getFileName cd

generateCode' cd = do
  let header = getHeader cd
  in
  writeHeader header $ getFileName cd -- FIXME : take cd instead of header/fileName
  res <- writeInstructions cd
  return res
  finished $ getFileName cd

-- FIXME : move to the where
corFileName fileName = (take (length(fileName) - 2) fileName) ++ ".cor"

-- FIXME : extract progSize from cd and add it to Header
generateCode cd = do
  res <- generateCode' $ resetByteCounter (setFileName cd fileName)
  return res
  where fileName = corFileName $ getFileName cd
