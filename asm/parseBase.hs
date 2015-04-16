module ParseBase (
  parseString,
  parseNum,
  parseId
) where

import Data.Char

parseString :: [Char] -> Bool
parseString token = (head token == '"') && (last token == '"')

identifierChar :: Char -> Bool
identifierChar c = (((ord(c) >= ord('a')) && (ord(c) <= ord('z'))) || c == '_') || numChar c

numChar :: Char -> Bool
numChar c = ((ord(c) >= ord('0')) && (ord(c) <= ord('9')))

parseNum :: [Char] -> (Bool, [Char])
parseNum candidate
  | head candidate == '-' =
    (length(filter (not . numChar) $ tail candidate) == 0, candidate)
  | otherwise =
    (length(filter (not . numChar) candidate) == 0, candidate)

parseId :: [Char] -> (Bool, [Char])
parseId candidate = (length(filter ( not . identifierChar) candidate) == 0, candidate)
