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
import ChampionData

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

-- FIXME : here is next
parseInstruction tokens
  | (length tokens) > 0 = parseInstruction' tokens
  | (length tokens) == 0 = True

worked True cd = (True, cd)
worked False cd =
  error $ "Syntax error \""
  ++ (getCurrentLine cd)
  ++ "\" (line "
  ++ (show $ (getLineNbr cd) + 1) ++ ")"

parseLines' [line] cd = worked (parseInstruction $ words line)(setCurrentLine cd line)
parseLines' (lineHead:lineTail) cd =
  (headRes && tailRes, tailD)
  where (headRes, headD) = worked (parseInstruction $ words lineHead) uCd
        (tailRes, tailD) = parseLines' lineTail (incLineNbr headD)
        uCd = setCurrentLine cd lineHead

parseLines lines cd = parseLines' lines $ setCurrentLine cd (head lines)

finished fileName (True, d) = putStrLn $ "Compilation complete for " ++ fileName
finished fileName (False, d) = putStrLn $ "Compilation failed for " ++ fileName

generateChampion fileName = do
  championFile <- openFile fileName ReadMode
  content <- hGetContents championFile
  let cd = newChampionData in
    finished fileName $ parseLines (lines content) cd

-- FIXME : change generateCode to act on cd not lines
--  generateCode $ lines content
  hClose championFile
