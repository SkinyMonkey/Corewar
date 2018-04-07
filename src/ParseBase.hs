module ParseBase where

import Data.Maybe
import Data.Char
import Utils

empty :: String -> Bool
empty = all isSpace

parseString :: String -> Maybe String
parseString token =
  let stringContent = slice 1 (length token - 1) token
  in if not (null stringContent) && head token == '"' && last token == '"'
     then Just stringContent
     else Nothing

alphaChar :: Char -> Bool
alphaChar c = (ord c >= ord 'a') && (ord c <= ord 'z')

numChar :: Char -> Bool
numChar c = (ord c >= ord '0') && (ord c <= ord '9')

identifierChar :: Char -> Bool
identifierChar c = alphaChar c || numChar c || c == '_'

parseNum :: String -> Maybe String
parseNum candidate
  | candidate == "" = Nothing
  | head candidate == '-' =
    if not (any (not . numChar) (tail candidate))
    then Just candidate
    else Nothing
  | otherwise =
    if not (any (not . numChar) candidate)
    then Just candidate
    else Nothing

parseId :: String -> Maybe String
parseId "" = Nothing
parseId candidate =
  let (c:cs) = candidate
  in if alphaChar c && not (any ( not . identifierChar) cs)
     then Just candidate
     else Nothing

-- FIXME : find a more elegant solution
solve check value =
  if check value
  then Just value
  else Nothing

solved :: (a, b) -> a
solved = fst

resolve :: a -> (Bool, a)
resolve x = (True, x)

reject :: a -> (Bool, a)
reject x = (False, x)
