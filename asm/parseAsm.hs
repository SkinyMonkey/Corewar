module ParseAsm (
  generateChampion
) where

import System.IO
import Data.List

import Op
import Utils
import ParseBase
import CheckArgs
import CodeGeneration

parseMetadata field cstring = (parseId $ tail field) && (parseString cstring)

parseLabel candidate =  parseId $ take (length(candidate) - 1) candidate

parseOp' candidate args = rightArgsNbr op args && rightTypes op args
  where op = byMnemonic candidate

parseOp candidate args
  | length(args) > 0 = parseOp' candidate (wordsWhen (==',') $ head args)
  | length(args) == 0 = error "No argument given"

parseInstruction' (token:args)
  | head token == '#' = True
  | head token == '.' = parseMetadata token $ intercalate "" args
  | last token == ':' = parseLabel token
    && ((length(args) > 0 && (parseOp (head args) (tail args))) || True)
  | otherwise = parseOp token args

parseInstruction tokens
  | (length tokens) > 0 = parseInstruction' tokens
  | (length tokens) == 0 = True

worked index line True d = (True,d)
worked index line False d =
  error $ "Syntax error \"" ++ line ++ "\" (line " ++ (show $ index + 1) ++ ")"

-- Execution will stop if anything is False
-- Now modify parseInstruction to take d to and use it
parseLines' [line] index d = worked index line (parseInstruction $ words line) d
parseLines' (lineHead:lineTail) index d =
  (headRes && tailRes, tailD)
  where 
    (headRes, headD) = worked index lineHead (parseInstruction $ words lineHead) d
    (tailRes, tailD) = parseLines' lineTail (index + 1) headD

parseLines lines = parseLines' lines 0 []

finished fileName (True, d) = putStrLn $ "Compilation complete for " ++ fileName
finished fileName (False, d) = putStrLn $ "Compilation failed for " ++ fileName

generateChampion fileName = do
  championFile <- openFile fileName ReadMode
  content <- hGetContents championFile
  finished fileName $ parseLines $ lines content
  generateCode $ lines content
  hClose championFile
