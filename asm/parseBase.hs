module ParseBase (
  parseString,
  parseNum,
  parseId
) where

import Data.Char

parseString token = (head token == '"') && (last token == '"')

identifierChar c = (((ord(c) >= ord('a')) && (ord(c) <= ord('z'))) || c == '_') || numChar c

numChar c = ((ord(c) >= ord('0')) && (ord(c) <= ord('9')))

parseNum candidate
  | head candidate == '-' = length(filter (not . numChar) $ tail candidate) == 0
  | otherwise = length(filter (not . numChar) candidate) == 0

parseId candidate = length(filter ( not . identifierChar) candidate) == 0


