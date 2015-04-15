module CodeGeneration (
  generateCode
) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Binary.Put
import Data.List

import Utils
import Op
import Header

serializeDirect value = do
  putWord32be value

serializeRegister value = do
  putWord32be value

serializeIndirect value = do
  putWord16be value

serializeOpCode opCode = do
  putWord8 opCode

serializeInstructionCode instructionCode = do
  putWord8 instructionCode

serializeParameter :: Put
serializeParameter = do
    serializeDirect 1
    serializeIndirect 2
 
putSerialize code = do
  B.appendFile "tests/res.cor" $ B.concat $ BL.toChunks $ runPut (serializeOpCode code)
--  B.appendFile "tests/res.cor" $ B.concat $ BL.toChunks $ runPut serializeParameter

-- FIXME : add encoding based on arg type
encodeDescription (p1:p2:p3:p4:[]) = "p4"
encodeDescription (p1:p2:p3:[]) = "p3"
encodeDescription (p1:p2:[]) = "p2"
encodeDescription (p1:[]) = "p1"
encodeDescription [] = "no p"

generateMetadata token args = generateHeader "test" "comment" 100

generateLabel token args = putStr ""

generateOp token args = putStrLn "gOpcode"-- putSerialize $ getCode op
--                        putStrLn $ token
--                        ++ " "
--                        ++ show args
--                        ++ " "
--                        ++ encodeDescription (wordsWhen (==',') $ head args)
--  | token == "live" || token == "zjmp" || token == "fork" || token == "lfork"
--    = putStrLn $ token ++ " " ++ (show $ getCode op)
--  | otherwise = putStrLn $ token ++ " " ++ encodeDescription args
  where op = byMnemonic token

generateInstruction [] = putStr ""
generateInstruction (token:args)
  | head token == '#' = putStr ""
  | head token == '.' = generateMetadata token $ intercalate "" args
  | last token == ':' = do
--      generateLabel token -- FIXME : needed?
      if length(args) > 0
      then generateOp (head args) (tail args)
      else putStr ""
  | otherwise = generateOp token args

generateCode' lines index
  | index < length(lines) = do
      let line = lines !! index in
        generateInstruction (words line)
      generateCode' lines (index + 1)
  | index == length(lines) = do
      putStrLn "Code generated"

generateCode lines = do
      generateCode' lines 0
