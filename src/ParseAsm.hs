module ParseAsm where

import Data.Char (isSpace)
import Data.List
import Data.Maybe

import Op
import Utils
import ParseBase
import CheckArgs
import ChampionData

-- FIXME : 
-- Either an error string or a ChampionData?
-- type ParseResult = Either String ChampionData
type ParseResult = Maybe ChampionData

parseMetadata :: [String] -> ChampionData -> ParseResult
parseMetadata (key:args) championData =
  let cstring = unwords args
      firstChar = head key
      field = parseId $ tail key -- removing the '.' char
      stringContent = parseString cstring
  in
  if firstChar == '.' && msolved field && msolved stringContent
  then Just $ addMetadata championData (fromJust field) (fromJust stringContent)
  else Nothing
parseMetadata [] _ = Nothing

parseLabel :: String -> ChampionData -> ParseResult
parseLabel token championData =
  let label = parseId $ init token -- removing the ':' char
  in if last token == ':' && msolved label
     then Just $ addLabel championData (fromJust label)
     else Nothing -- FIXME : Nothing? or championData, as in 'untouched'?

dropComments :: [String] -> [String]
dropComments args = 
  let isComment arg = any (==';') arg || any (=='#') arg
      commentIndex = findIndex isComment args
  in if isJust commentIndex
     then take (fromJust commentIndex) args
     else args

parseOp :: [String] -> ChampionData -> ParseResult
parseOp (candidate:args) championData =
  let op = byMnemonic candidate
      argTypes = checkArgTypes op args
  in if rightArgsNbr op args championData -- FIXME : finish
     then Just $ addInstruction championData op argTypes
     else Nothing

splitOnCommas :: [String] -> [String]
splitOnCommas = concatMap $ wordsWhen (==',')

-- TODO : remove the # guard
-- TODO : split into if thens?
-- les clean to read but better code
parseInstruction :: [String] -> ChampionData -> ParseResult
parseInstruction (token:args) championData
--  | head token == '#' = Just championData                  -- comment
  | head token == '.' = parseMetadata tokens championData  -- metadata
  | last token == ':' = parseLabel'                        -- label
  | otherwise = parseOp tokens championData                -- op
    where args' = splitOnCommas args
          tokens = token:args'
          parseLabel' = if not (null args')
                        then parseOp args' $ fromJust $ parseLabel token championData
                        else parseLabel token championData
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
      cleanedLine = ( dropComments . words ) line
      currentResult = if ((not . null) $ unwords cleanedLine) &&
                         ((not . empty) $ unwords cleanedLine)
                      then parseInstruction cleanedLine updatedChampionData
                      else Just updatedChampionData
  in worked (Just updatedChampionData) currentResult

parseLines :: [String] -> ChampionData -> ParseResult
parseLines contentLines championData = foldl parseLine (Just championData) contentLines

parseChampion :: String -> String -> ParseResult
parseChampion fileName content =
  parseLines (lines content) (newChampionData fileName)
