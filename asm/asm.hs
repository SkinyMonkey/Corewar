import System.IO
import System.Environment (getArgs)
import Data.Char
import Data.List

import Op
import Utils

parseString token = (head token == '"') && (last token == '"')

identifierChar c = (((ord(c) >= ord('a')) && (ord(c) <= ord('z'))) || c == '_') || numChar c

numChar c = ((ord(c) >= ord('0')) && (ord(c) <= ord('9')))

parseNum candidate
  | head candidate == '-' = length(filter (not . numChar) $ tail candidate) == 0
  | otherwise = length(filter (not . numChar) candidate) == 0

parseId candidate = length(filter ( not . identifierChar) candidate) == 0

parseMetadata field cstring = (parseId $ tail field) && (parseString cstring)

parseLabel candidate =  parseId $ take (length(candidate) - 1) candidate

parseOp candidate args = parseOp' candidate (wordsWhen (==',') $ head args)

rightArgsNbr op args = (getNbrArgs op == argsNbr
                          || (error $ "Bad # of args for mnemonic \""
                             ++ getMnemonic op
                             ++ "\": "
                             ++ (show argsNbr)))
  where argsNbr = length(args)

register candidate = head candidate == 'r' && parseNum (tail candidate)

direct candidate = head candidate == '%' && indirect (tail candidate)

indirect candidate = label candidate || parseNum candidate

label candidate = head candidate == ':' && parseId (tail candidate)

-- For each arg, check it's of the right type
rightTypes op args = True

-- FIXME : finish
parseOp' candidate args = rightArgsNbr op args && rightTypes op args
  where op = byMnemonic candidate

parseInstruction_ (token:args)
  | head token == '#' = True
  | head token == '.' = parseMetadata token $ intercalate "" args
  | last token == ':' = parseLabel token && (length(args) > 0 && ((parseOp (head args) (tail args))) || True)
  | otherwise = parseOp token args

parseInstruction tokens
  | (length tokens) > 0 = parseInstruction_ tokens
  | (length tokens) == 0 = True

worked index line True = print "Rule worked"
worked index line False = print $ "Rule did not work \"" ++ line ++ "\"(line " ++ (show index) ++ ")"

--parseLine line = do

parseLine' lines index
  | index < length(lines) = do
      let line = lines !! index in
        worked index line $ parseInstruction $ words line
      parseLine' lines (index + 1)
  | index == length(lines) = print "End of file"

parseLine lines =
  parseLine' lines 0

parseChampion fileName = do
  championFile <- openFile fileName ReadMode
  content <- hGetContents championFile
--  mapM_ parseLine $ lines content
  parseLine $ lines content
  hClose championFile

main :: IO ()
main = do
        args <- getArgs
        mapM_ parseChampion args
