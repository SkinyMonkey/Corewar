module CodeGeneration (
  generateCode
) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import Data.Binary.Put

import Op
import Header
import ChampionData
import Data.Word
import Data.Bits

corFileName fileName = (take (length(fileName) - 2) fileName) ++ ".cor"

serializeRegister register = do
  putWord32be register

serializeDirect direct = do
  putWord32be direct

serializeIndirect indirect = do
  putWord16be indirect
--  putWord32be indirect

-- FIXME : mod to transform label into indirect?
serializeLabel label = do
  putWord32be label

serializeOpCode opCode = do
  putWord8 opCode

serializeInstructionCode instructionCode = do
  putWord8 instructionCode

-- 01 register
-- 10 direct
-- 11 indirect
encode parameterType parameter index
  | parameterType == register = shiftL 0x01 index
  | parameterType == direct = shiftL 0x02 index
  | parameterType == indirect = shiftL 0x03 index
  | otherwise = error "Unknown parameter type : this should never happen."

generateOpCode' ((t1, p1):(t2, p2):(t3, p3):(t4, p4):[]) =
  encode t1 p1 6 .|. (encode t2 p2 4 .|. (encode t3 p3 2 .|. encode t4 p4 0))
generateOpCode' ((t1, p1):(t2, p2):(t3, p3):[]) =
  encode t1 p1 6 .|. (encode t2 p2 4 .|. encode t3 p3 2)
generateOpCode' ((t1, p1):(t2, p2):[]) =
  encode t1 p1 6 .|. encode t2 p2 4
generateOpCode' ((t1, p1):[]) = 
  encode t1 p1 6

generateOpCode' [] = error "No parameters : this should never happen."

generateOpCode instruction =
  generateOpCode' parameters
  where parameters = snd instruction

generateParameter parameter
  | parameterType == register = serializeRegister $ (read parameterValue :: Word32)
  | parameterType == direct = serializeDirect $ (read parameterValue :: Word32)
  | parameterType == indirect = serializeIndirect $ (read parameterValue :: Word16)
  | parameterType == label = serializeLabel $ (read parameterValue :: Word32)
  | otherwise = error "Unknown parameter type : this should never happen."
  where parameterType = fst parameter
        parameterValue = snd parameter

serializeParameters [] = error "No arguments given."

serializeParameters [parameter] = do 
  generateParameter parameter

serializeParameters (headParameter:tailParameters) = do
  generateParameter headParameter
  serializeParameters tailParameters

serializeInstruction instruction = do
  serializeInstructionCode instructionCode
  serializeOpCode opCode
  serializeParameters parameters
  where instructionCode = fst instruction
        opCode = (generateOpCode instruction) :: Word8
        parameters = snd instruction

writeInstruction' instruction fileName = do
  B.appendFile fileName
    $ B.concat
    $ BL.toChunks
    $ runPut
    $ serializeInstruction instruction
  return True

writeInstruction instruction fileName = do
  if not $ instructionCode `elem` noOpCodeInstructions
  then writeInstruction' instruction fileName
  else return True
  where instructionCode = fst instruction
        noOpCodeInstructions = [1, 9, 12, 15]

-- FIXME : replace by map?
writeInstructions' [] _ = do return True
writeInstructions' [instruction] fileName = do
  writeInstruction instruction fileName
  return True

-- FIXME : remove <- (useless?)
writeInstructions' (headInstruction:tailInstruction) fileName = do
  res <- writeInstruction headInstruction fileName
  return res
  writeInstructions' tailInstruction fileName

writeInstructions cd fileName = do
  res <- writeInstructions' instructions fileName
  return res
  where instructions = getInstructions cd

finished fileName = putStrLn $ "Generation complete for " ++ fileName
--finished (False, cd) = putStrLn $ "Generation failed for " ++ getFileName cd

generateCode cd = do
  writeHeader header fileName
  writeInstructions cd fileName
  finished fileName
  where header = getHeader cd
        fileName = corFileName $ getFileName cd
