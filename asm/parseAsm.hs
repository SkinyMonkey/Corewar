module ParseAsm (
  parseChampion,
  parseInstruction', -- DEBUG
) where

import Data.List

import Op
import Utils
import ParseBase
import CheckArgs
import ChampionData

-- FIXME : DEBUG
import Debug.Trace

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

parseLabel :: String -> ChampionData -> ParseResult
parseLabel label championData =
  if (solved $ parseId $ label)
  then resolve $ addLabel championData label
  else reject championData
parseLabel _ championData = reject championData

parseOp' :: String -> [String] -> ChampionData -> ParseResult
parseOp' candidate args championData =
  let op = byMnemonic candidate
      (validTypes, argTypes) = checkArgTypes op args
  in
  if (rightArgsNbr op args championData && validTypes)
  then resolve $ addInstruction championData op argTypes
  else reject $ championData

splitOnCommas :: [String] -> [String]
splitOnCommas = concat . (map $ wordsWhen (==','))

parseOp :: [String] -> ChampionData -> ParseResult
parseOp (candidate:args) championData
  | length(args) > 0 = parseOp' candidate (splitOnCommas args) championData
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
          label = init token
          parseLabel' = if length (args) > 0
                        then parseOp args $ snd $ parseLabel label championData
                        else parseLabel label championData
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

parseLines' :: ParseResult -> String -> ParseResult
parseLines' (parseRes, championData) line  = 
  let updatedChampionData = incLineNbr $ setCurrentLine championData line
      currentResult = parseInstruction (trace' line) updatedChampionData
      (currentParseRes, championData') = currentResult
  in worked $ if currentParseRes
              then currentResult
              else (parseRes, updatedChampionData)

parseLines :: [String] -> ChampionData -> ParseResult
parseLines lines championData = foldl parseLines' (resolve championData) lines

parseChampion :: String -> String -> ParseResult
parseChampion fileName content =
  parseLines (lines content) (newChampionData fileName)
