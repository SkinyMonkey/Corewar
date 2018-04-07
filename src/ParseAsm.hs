module ParseAsm where

import Data.Char (isSpace)
import Data.List
import Data.Maybe
import Data.Either

import Op
import Utils
import ParseBase
import CheckArgs
import ChampionData

type ParseError = String
type ParseResult = Either ParseError ChampionData

parseMetadata :: [String] -> ChampionData -> ParseResult
parseMetadata [] _ = Left "Empty line"
parseMetadata (key:args) championData =
  let cstring = unwords args
      firstChar = head key
      field = parseId $ tail key -- removing the '.' char
      stringContent = parseString cstring
  in
  if firstChar == '.' && isJust field && isJust stringContent
  then Right $ addMetadata championData (fromJust field) (fromJust stringContent)
  else Left "Malformed metadata"

parseLabel :: String -> ChampionData -> ParseResult
parseLabel [] _ = Left "Empty line"
parseLabel token championData =
  let label = parseId $ init token -- removing the ':' char
  in if last token == ':' && isJust label
     then Right $ addLabel championData (fromJust label)
     else Left "Malformed label"

dropComments :: [String] -> [String]
dropComments args = 
  let isComment arg = elem ';' arg || elem '#' arg
      commentIndex = findIndex isComment args
  in if isJust commentIndex
     then take (fromJust commentIndex) args
     else args

parseOp :: [String] -> ChampionData -> ParseResult
parseOp [] _ = Left "Empty line"
parseOp (candidate:args) championData =
  let op = byMnemonic candidate
      argTypes = checkArgTypes op args championData
   in flip fmap argTypes $ addInstruction championData op

splitOnCommas :: [String] -> [String]
splitOnCommas = concatMap $ wordsWhen (==',')

parseInstruction :: [String] -> ChampionData -> ParseResult
parseInstruction (token:args) championData
  | head token == '.' = parseMetadata tokens championData  -- metadata
  | last token == ':' = parseLabel'                        -- label
  | otherwise = parseOp tokens championData                -- op
    where args' = splitOnCommas args
          tokens = token:args'
          parseLabel' = if not (null args')
                        then parseOp args' $ fromRight championData $ parseLabel token championData
                        else parseLabel token championData
parseInstruction [] _ = Left "Empty line"

parseLine :: (String, ChampionData) -> String -> (String, ChampionData)
parseLine (errors, championData) line  =
  let updatedChampionData = incLineNbr $ setCurrentLine championData line
      cleanedLine = ( dropComments . words ) line
      currentResult = if (not . null) (unwords cleanedLine) &&
                         (not . empty) (unwords cleanedLine)
                      then parseInstruction cleanedLine updatedChampionData
                      else Right updatedChampionData
  in case currentResult of
    Right result -> (errors, result)
    Left error   -> (errors ++ "\n" ++ error, updatedChampionData)

parseChampion :: String -> String -> ParseResult
parseChampion fileName content =
  let championData = newChampionData fileName
      parseLines = foldl parseLine ("", championData)
      splitContent = lines content
      (errors, updatedChampionData) = parseLines splitContent
  in if not (null errors)
     then Left errors
     else Right updatedChampionData
