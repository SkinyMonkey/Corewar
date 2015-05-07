module ParseAsm (
  parseChampion
) where

import System.IO
import Data.List

import Op
import Utils
import ParseBase
import CheckArgs
import ChampionData

parseMetadata field cstring cd =
  if (fst $ parseId $ tail field) && (parseString cstring)
  then (True, addMetadata cd (tail field) cstring)
  else (False, cd)

parseLabel candidate cd =
  if (fst $ parseId $ take (length(candidate) - 1) candidate)
  then (True, addLabel cd candidate)
  else (False, cd)

parseOp' candidate args cd =
  if (rightArgsNbr op args && validTypes)
  then (True, addInstruction cd op argTypes)
  else (False, cd)
  where op = byMnemonic candidate
        (validTypes, argTypes) = retrieveTypes op args

parseOp candidate args cd
  | length(args) > 0 = parseOp' candidate (wordsWhen (==',') $ head args) cd
  | length(args) == 0 =
    error $ "No argument given to " ++ candidate ++ " line " ++ (show $ getLineNbr cd)
parseOp _ _ cd = (False, cd)

parseInstruction' [] cd = (True, cd)
parseInstruction' (token:args) cd
  | head token == '#' = (True, cd)
  | head token == '.' = parseMetadata token (intercalate "" args) cd
  | last token == ':' = (headRes && tailRes, tailCd)
  | otherwise = parseOp token args cd
    where (headRes, headCd) = parseLabel token cd
          (tailRes, tailCd) =
            if (length(args) > 0)
            then parseOp (head args) (tail args) headCd
            else (True, headCd)
 
parseInstruction tokens cd
  | (length tokens) > 0 = parseInstruction' (words tokens) uCd
  | (length tokens) == 0 = (True, uCd)
  where uCd = setCurrentLine cd tokens
parseInstruction _ cd = (False, cd)

worked (True, cd) = (True, cd)
worked (False, cd) =
  error $ "Syntax error \""
  ++ (getCurrentLine cd)
  ++ "\" (line "
  ++ (show $ (getLineNbr cd) + 1) ++ ")"

parseLines' [line] cd = worked (parseInstruction line cd)
parseLines' (lineHead:lineTail) cd =
  (headRes && tailRes, tailCd)
  where (headRes, headCd) = worked (parseInstruction lineHead cd)
        (tailRes, tailCd) = parseLines' lineTail (incLineNbr headCd)
parseLines' [] cd = (False, cd)

parseLines lines cd = parseLines' lines $ setCurrentLine cd (head lines)

finished (True, cd) = do
  putStrLn $ "Compilation complete for " ++ getFileName cd
  return cd
finished (False, cd) = error $ "Compilation failed for " ++ getFileName cd

parseChampion fileName = do
  championFile <- openFile fileName ReadMode
  content <- hGetContents championFile
  res <- finished $ parseLines (lines content) (newChampionData fileName)
  hClose championFile
  return res
