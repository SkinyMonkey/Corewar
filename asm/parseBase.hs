module ParseBase (
  parseString,
  parseNum,
  parseId,
  resolve,
  reject,
  solved,
) where

import Data.Char

parseString :: String -> Bool
parseString token = (head token == '"') && (last token == '"')

identifierChar :: Char -> Bool
identifierChar c = (((ord c >= ord 'a') && (ord c <= ord 'z')) || c == '_') || numChar c

numChar :: Char -> Bool
numChar c = (ord c >= ord '0') && (ord c <= ord '9')

parseNum :: String -> (Bool, String)
parseNum candidate
  | head candidate == '-' =
    (not (any (not . numChar) (tail candidate)), candidate)
  | otherwise =
    (not (any (not . numChar) candidate), candidate)

parseId :: String -> (Bool, String)
parseId candidate = (not (any ( not . identifierChar) candidate), candidate)

solved :: (a, b) -> a
solved = fst

resolve :: a -> (Bool, a)
resolve x = (True, x)

reject :: a -> (Bool, a)
reject x = (False, x)
