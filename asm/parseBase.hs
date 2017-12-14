module ParseBase (
  parseString,
  parseNum,
  parseId,
  resolve,
  reject,
  solved,
  trace'
) where

import Data.Char

-- FIXME : DEBUG
import Debug.Trace

trace' x = trace ("$>" ++ (show x)) x

parseString :: [Char] -> Bool
parseString token = (head token == '"') && (last token == '"')

identifierChar :: Char -> Bool
identifierChar c = (((ord(c) >= ord('a')) && (ord(c) <= ord('z'))) || c == '_') || numChar c

numChar :: Char -> Bool
numChar c = ((ord(c) >= ord('0')) && (ord(c) <= ord('9')))

parseNum :: [Char] -> (Bool, String)
parseNum candidate
  | head candidate == '-' =
    (length(filter (not . numChar) $ tail candidate) == 0, candidate)
  | otherwise =
    (length(filter (not . numChar) candidate) == 0, candidate)

parseId :: [Char] -> (Bool, [Char])
parseId candidate = (length(filter ( not . identifierChar) candidate) == 0, candidate)

solved :: (a, b) -> a
solved = fst

resolve :: a -> (Bool, a)
resolve x = (True, x)

reject :: a -> (Bool, a)
reject x = (False, x)
