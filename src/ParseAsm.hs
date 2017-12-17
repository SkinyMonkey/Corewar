module ParseAsm where

import Data.List
import Data.Maybe

import Op
import Utils
import ParseBase
import CheckArgs
import ChampionData

type ParseResult = (Bool, ChampionData)

parseMetadata :: [String] -> ChampionData -> ParseResult
parseMetadata (key:args) championData =
  let cstring = unwords args
      field = parseId $ tail key -- removing the '.' char
      stringContent = parseString cstring
  in
  if msolved field && msolved stringContent
  then resolve $ addMetadata championData (fromJust field) (fromJust stringContent)
  else reject championData
parseMetadata _ championData = reject championData

parseLabel :: String -> ChampionData -> ParseResult
parseLabel token championData =
  let label = parseId token
  in if msolved label
     then resolve $ addLabel championData (fromJust label)
     else reject championData

dropComments :: [String] -> [String]
dropComments args = 
  let isComment arg = any (==';') arg
      commentIndex = findIndex isComment args
  in case commentIndex of
     Just index -> take index args
     Nothing -> args

parseOp' :: String -> [String] -> ChampionData -> ParseResult
parseOp' candidate args championData =
  let op = byMnemonic candidate
      noCommentArgs = dropComments args
      (validTypes, argTypes) = checkArgTypes op noCommentArgs
  in
  if rightArgsNbr op noCommentArgs championData && validTypes
  then resolve $ addInstruction championData op argTypes
  else reject championData

splitOnCommas :: [String] -> [String]
splitOnCommas = concatMap $ wordsWhen (==',')

parseOp :: [String] -> ChampionData -> ParseResult
parseOp (candidate:args) championData
  | not (null args) = parseOp' candidate (splitOnCommas args) championData
  | null args =
    error $ "No argument given to " ++ candidate ++ " line " ++ show (getLineNbr championData)
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
          parseLabel' = if not (null args)
                        then parseOp args $ snd $ parseLabel label championData
                        else parseLabel label championData
parseInstruction' _ championData = reject championData

parseInstruction :: String -> ChampionData -> ParseResult
parseInstruction line championData
  | null line = resolve championData -- the line is empty
  | not (null line) = let tokens = words line
                      in  parseInstruction' tokens championData
parseInstruction _ championData = reject championData

-- TODO : remove worked in favor of error accumulation
worked :: ParseResult -> ParseResult
worked (False, championData) =
  error $ "Syntax error \""
  ++ getCurrentLine championData
  ++ "\" (line "
  ++ show (getLineNbr championData + 1) ++ ")"
worked x = x

parseLines' :: ParseResult -> String -> ParseResult
parseLines' (parseRes, championData) line  =
  let updatedChampionData = incLineNbr $ setCurrentLine championData line
      currentResult = parseInstruction line updatedChampionData
      (currentParseRes, _) = currentResult
  in worked $ if currentParseRes
              then currentResult
              else (parseRes, updatedChampionData)

parseLines :: [String] -> ChampionData -> ParseResult
parseLines contentLines championData = foldl parseLines' (resolve championData) contentLines

parseChampion :: String -> String -> ParseResult
parseChampion fileName content =
  parseLines (lines content) (newChampionData fileName)
