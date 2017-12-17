module ParseAsm where

import Data.List
import Data.Maybe

import Op
import Utils
import ParseBase
import CheckArgs
import ChampionData

type ParseResult = Maybe ChampionData

-- data ParseResult = Ok ChampionData | Error ChampionData

parseMetadata :: [String] -> ChampionData -> ParseResult
parseMetadata (key:args) championData =
  let cstring = unwords args
      field = parseId $ tail key -- removing the '.' char
      stringContent = parseString cstring
  in
  if msolved field && msolved stringContent
  then Just $ addMetadata championData (fromJust field) (fromJust stringContent)
  else Nothing
parseMetadata [] _ = Nothing

parseLabel :: String -> ChampionData -> ParseResult
parseLabel token championData =
  let label = parseId token
  in if msolved label
     then Just $ addLabel championData (fromJust label)
     else Nothing

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
  then Just $ addInstruction championData op argTypes
  else Nothing

splitOnCommas :: [String] -> [String]
splitOnCommas = concatMap $ wordsWhen (==',')

parseOp :: [String] -> ChampionData -> ParseResult
parseOp (candidate:args) championData
  | not (null args) = parseOp' candidate (splitOnCommas args) championData
  | null args =
    error $ "No argument given to " ++ candidate ++ " line " ++ show (getLineNbr championData)
parseOp [] _ = Nothing

-- FIXME : how to compose parseOp with parseLabel if needed?
parseInstruction' :: [String] -> ChampionData -> ParseResult
parseInstruction' (token:args) championData
  | head token == '#' = Just championData                  -- comment
  | head token == '.' = parseMetadata tokens championData  -- metadata
  | last token == ':' = parseLabel'                        -- label
  | otherwise = parseOp tokens championData                -- op
    where tokens = token:args
          label = init token
          parseLabel' = if not (null args)
                        then parseOp args $ fromJust $ parseLabel label championData
                        else parseLabel label championData
parseInstruction' [] _ = Nothing

parseInstruction :: String -> ChampionData -> ParseResult
parseInstruction line championData
  | null line = Just championData -- the line is empty
  | not (null line) = let tokens = words line
                      in  parseInstruction' tokens championData
parseInstruction [] _ = Nothing

-- TODO : remove worked in favor of error accumulation?
worked :: ParseResult -> ParseResult -> ParseResult
worked _ (Just championData) = Just championData -- current result one
worked (Just championData) Nothing = -- default one with line updated
  error $ "Syntax error \""
  ++ getCurrentLine championData
  ++ "\" (line "
  ++ show (getLineNbr championData + 1) ++ ")"

parseLine :: ParseResult -> String -> ParseResult
parseLine (Just championData) line  =
  let updatedChampionData = incLineNbr $ setCurrentLine championData line
      currentResult = parseInstruction line updatedChampionData
  in worked (Just updatedChampionData) currentResult

parseLines :: [String] -> ChampionData -> ParseResult
parseLines contentLines championData = foldl parseLine (Just championData) contentLines

parseChampion :: String -> String -> ParseResult
parseChampion fileName content =
  parseLines (lines content) (newChampionData fileName)
