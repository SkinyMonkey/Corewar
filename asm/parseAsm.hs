module ParseAsm (
  parseChampion
) where

import Data.List

import Op
import Utils
import ParseBase
import CheckArgs
import ChampionData

-- FIXME : DEBUG
--import Debug.Trace

type ParseResult = (Bool, ChampionData)

parseMetadata :: [String] -> ChampionData -> ParseResult
parseMetadata (key:args) championData =
  let cstring = intercalate " " args
      field = tail key -- removing the '.' char
  in
  if (solved $ parseId field) && (parseString cstring)
  then resolve $ addMetadata championData field cstring
  else reject championData
parseMetadata _ championData = reject championData

parseLabel :: [String] -> ChampionData -> ParseResult
parseLabel (candidate:_) championData =
  let label = init candidate
  in
  if (solved $ parseId label)
  then resolve $ addLabel championData label
  else reject championData
parseLabel _ championData = reject championData

parseOp' :: String -> [String] -> ChampionData -> ParseResult
parseOp' candidate args championData =
  let op = byMnemonic candidate
      (validTypes, argTypes) = checkArgTypes op args
  in
  if (rightArgsNbr op args && validTypes)
  then resolve $ addInstruction championData op argTypes
  else reject $ championData

parseOp :: [String] -> ChampionData -> ParseResult
parseOp (candidate:args) championData
  | length(args) > 0 = parseOp' candidate (wordsWhen (==',') $ head args) championData
  | length(args) == 0 =
    error $ "No argument given to " ++ candidate ++ " line " ++ (show $ getLineNbr championData)
parseOp _ championData = reject championData

-- FIXME : how to compose parseOp with parseLabel if needed?
parseInstruction' :: [String] -> ChampionData -> ParseResult
parseInstruction' (token:args) championData
  | head token == '#' = resolve championData               -- comment
  | head token == '.' = parseMetadata tokens championData  -- metadata
  | last token == ':' = parseLabel'                        -- label
  | otherwise = parseOp tokens championData                -- op
    where tokens = token:args
          parseLabel' = let (_:args) = tokens
                        in if length (args) > 0
                           then parseOp tokens $ snd $ parseLabel tokens championData
                           else parseLabel tokens championData
parseInstruction' _ championData = reject championData

parseInstruction :: String -> ChampionData -> ParseResult
parseInstruction line championData
  | (length line) == 0 = resolve championData -- the line is empty
  | (length line) > 0 = let tokens = words line
                        in  parseInstruction' tokens championData
parseInstruction _ championData = reject championData

-- TODO : remove worked in favor of error accumulation
worked :: ParseResult -> ParseResult
worked (False, championData) =
  error $ "Syntax error \""
  ++ (getCurrentLine championData)
  ++ "\" (line "
  ++ (show $ (getLineNbr championData) + 1) ++ ")"
worked x = x

parseLines' :: String -> ParseResult -> ParseResult
parseLines' line (parseRes, championData) = 
  let updatedChampionData = setCurrentLine championData line
      (currentParseRes, championData') = parseInstruction line updatedChampionData
  in worked $ (parseRes && currentParseRes, championData')

parseLines :: [String] -> ChampionData -> ParseResult
parseLines lines championData = foldr parseLines' (resolve championData) lines

parseChampion :: String -> String -> ParseResult
parseChampion fileName content =
  parseLines (lines content) (newChampionData fileName)
