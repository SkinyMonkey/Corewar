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

-- FIXME : 
-- Either an error string or a ChampionData?
-- type ParseResult = Either String ChampionData
type ParseResult = Maybe ChampionData

type ParseError = String
type ParseResultError = Either ParseError ChampionData

parseMetadata :: [String] -> ChampionData -> ParseResultError
parseMetadata (key:args) championData =
  let cstring = unwords args
      firstChar = head key
      field = parseId $ tail key -- removing the '.' char
      stringContent = parseString cstring
  in
  if firstChar == '.' && msolved field && msolved stringContent
  then Right $ addMetadata championData (fromJust field) (fromJust stringContent)
  else Left "Malformed metadata"
parseMetadata [] _ = Left "Empty line"

parseLabel :: String -> ChampionData -> ParseResultError
parseLabel token championData =
  let label = parseId $ init token -- removing the ':' char
  in if last token == ':' && msolved label
     then Right $ addLabel championData (fromJust label)
     else Left "Malformed label"
parseLabel [] _ = Left "Empty line"

dropComments :: [String] -> [String]
dropComments args = 
  let isComment arg = any (==';') arg || any (=='#') arg
      commentIndex = findIndex isComment args
  in if isJust commentIndex
     then take (fromJust commentIndex) args
     else args

parseOp :: [String] -> ChampionData -> ParseResultError
parseOp (candidate:args) championData =
  let op = byMnemonic candidate
      argTypes = checkArgTypes op args
  in if rightArgsNbr op args championData -- FIXME : finish
     then Right $ addInstruction championData op argTypes
     else Left "Malformed op, bad number of args" -- FIXME : return checkArgTypes result
parseOp [] _ = Left "Empty line"

splitOnCommas :: [String] -> [String]
splitOnCommas = concatMap $ wordsWhen (==',')

parseInstruction :: [String] -> ChampionData -> ParseResultError
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

-- TODO : remove worked in favor of error accumulation?
worked :: ParseResultError -> ParseResultError -> ParseResult
worked _ (Right championData) = Just championData 
worked (Right championData) (Left err) = error err-- default one with line updated
--  error $ "Syntax error \""
--  ++ getCurrentLine championData
--  ++ "\" (line "
--  ++ show (getLineNbr championData + 1) ++ ")"


-- worked _ (Just championData) = Just championData -- current result one
-- worked (Just championData) Nothing = -- default one with line updated
--   error $ "Syntax error \""
--   ++ getCurrentLine championData
--   ++ "\" (line "
--   ++ show (getLineNbr championData + 1) ++ ")"

parseLine :: ParseResult -> String -> ParseResult
parseLine (Just championData) line  =
  let updatedChampionData = incLineNbr $ setCurrentLine championData line
      cleanedLine = ( dropComments . words ) line
      currentResult = if ((not . null) $ unwords cleanedLine) &&
                         ((not . empty) $ unwords cleanedLine)
                      then parseInstruction cleanedLine updatedChampionData
                      else Right updatedChampionData
  in worked (Right updatedChampionData) currentResult

parseLines :: [String] -> ChampionData -> ParseResult
parseLines contentLines championData = foldl parseLine (Just championData) contentLines

parseChampion :: String -> String -> ParseResult
parseChampion fileName content =
  parseLines (lines content) (newChampionData fileName)
