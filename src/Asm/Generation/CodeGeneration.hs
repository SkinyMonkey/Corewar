{-# LANGUAGE BinaryLiterals #-}

module Asm.Generation.CodeGeneration where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import Data.Binary.Put
import Control.Monad (when)

import Numeric
import Data.Char
import Data.Word
import Data.Bits
import Foreign.Storable

import Op
import Asm.Header
import Asm.ChampionData

import Utils

printBinary :: Int -> String
printBinary value = "0b" ++ (lpad 8 $ showIntAtBase 2 intToDigit value "")
  where lpad m xs = let ys = take m xs
                    in replicate (m - length ys) '0' ++ ys

-- Example:
-- Register, Indirect, Direct
-- 1 << 6 | 3 << 4 | 2 << 2 | 1 << 0 == 0b01111001

-- 01 register
-- 10 direct
-- 11 indirect
encodeParameter :: (Int, Word8) -> Parameter -> (Int, Word8)
encodeParameter (index, opCode) parameter =
 let newValue = case parameter of
                   Register _ -> 0b00000001
                   Direct _ -> 0b00000010
                   Indirect _ -> 0b00000011
 in (index - 2, opCode .|. (shiftL newValue index))

generateOpCode :: [Parameter] -> Word8
generateOpCode parameters =
  let emptyByte = 0
      maximumIndex = 6
      (_, opCode) = foldl encodeParameter (maximumIndex, emptyByte) parameters
  in opCode

-- Labels are indirect now
serializeParameter :: ChampionData -> Word8 -> Parameter -> Put
serializeParameter championData instructionCode parameter =
  case parameter of
    Register parameterValue -> putWord8 (fromIntegral parameterValue :: Word8)
    Direct parameterValue -> if instructionCode `elem` haveIndexInstructions
                             then putWord16be (fromIntegral parameterValue :: Word16)
                             else putWord32be parameterValue
    Indirect parameterValue -> putWord16be (fromIntegral parameterValue :: Word16)
    _ -> error "Unknown parameter type : this should never happen."
 
-- FIXME : should return a buffer instead of an empty monad
--         foldl?
serializeParameters :: ChampionData -> Word8 -> [Parameter] -> PutM ()
serializeParameters championData instructionCode parameters = 
  mapM_ (serializeParameter championData instructionCode) parameters

serializeInstruction :: ChampionData -> EvaluatedInstruction -> Put
serializeInstruction championData (instructionCode, parameters) = do
  let opCode = generateOpCode parameters
  putWord8 instructionCode
  when (notElem instructionCode noOpCodeInstructions) $ putWord8 opCode
  serializeParameters championData instructionCode parameters

-- FIXME : why + 4? missing uint?
serializeHeader :: Header -> Put
serializeHeader header = do
  putByteString $ magic header
  putByteString $ B.pack $ rightPaddedString (progNameLength + 4) (progName header)
  putWord32be $ progSize header
  putByteString $ B.pack $ rightPaddedString (commentLength + 4) (Asm.Header.comment header)

writeInstruction :: String -> ChampionData -> EvaluatedInstruction -> IO ()
writeInstruction fileName championData instruction = do
  let serializedInstruction = runPut $ serializeInstruction championData instruction
  B.appendFile fileName $ B.concat $ BL.toChunks $ serializedInstruction

writeInstructions :: String -> ChampionData -> [EvaluatedInstruction] -> IO ()
writeInstructions fileName championData instructions =
  mapM_ (writeInstruction fileName championData) instructions

writeHeader :: String -> ChampionData -> IO ()
writeHeader fileName championData = do
  let header = getHeader championData
  B.writeFile fileName (B.concat $ BL.toChunks $ runPut (serializeHeader header))

writeChampion :: String -> ChampionData -> [EvaluatedInstruction] -> IO ()
writeChampion fileName championData instructions = do
   writeHeader fileName championData
   writeInstructions fileName championData instructions
