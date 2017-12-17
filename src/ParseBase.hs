module ParseBase where

import Data.Maybe
import Data.Char
import Utils

parseString :: String -> Maybe String
parseString token =
  let stringContent = slice 1 ((length token) - 1) token
  in if (head token == '"') && (last token == '"')
     then Just stringContent
     else Nothing

identifierChar :: Char -> Bool
identifierChar c = (((ord c >= ord 'a') && (ord c <= ord 'z')) || c == '_') || numChar c

numChar :: Char -> Bool
numChar c = (ord c >= ord '0') && (ord c <= ord '9')

parseNum :: String -> Maybe String
parseNum candidate
  | head candidate == '-' =
    if (not (any (not . numChar) (tail candidate)))
    then Just candidate
    else Nothing
  | otherwise =
    if (not (any (not . numChar) candidate))
    then Just candidate
    else Nothing

parseId :: String -> Maybe String
parseId candidate =
  if not (any ( not . identifierChar) candidate)
  then Just candidate
  else Nothing

-- FIXME : find a more elegant solution
solve check value =
  if check value
  then Just value
  else Nothing

msolved :: Maybe a -> Bool
msolved = isJust

solved :: (a, b) -> a
solved = fst

resolve :: a -> (Bool, a)
resolve x = (True, x)

reject :: a -> (Bool, a)
reject x = (False, x)
