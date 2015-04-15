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
  | last token == ':' = parseLabel token && ((length(args) > 0 && (parseOp (head args) (tail args))) || True)
  | otherwise = parseOp token args

parseInstruction tokens
  | (length tokens) > 0 = parseInstruction' tokens
  | (length tokens) == 0 = True

worked index line True = putStr ""
worked index line False = putStrLn $ "Syntax error \"" ++ line ++ "\" (line " ++ (show $ index + 1) ++ ")"

-- FIXME : redefine with guards checking for empty list and use tail
parseLine' lines index
  | index < length(lines) = do
      let line = lines !! index in
        worked index line $ parseInstruction $ words line
      parseLine' lines (index + 1)
  | index == length(lines) = do
      putStrLn "Syntax OK"

parseLine lines =
  parseLine' lines 0

finished True fileName = putStrLn $ "Compilation complete for " ++ fileName
finished False fileName = putStrLn $ "Compilation failed for " ++ fileName

generateChampion fileName = do
  championFile <- openFile fileName ReadMode
  content <- hGetContents championFile
  parseLine $ lines content
  generateCode $ lines content
  hClose championFile
