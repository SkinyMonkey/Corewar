import System.IO
import System.Environment (getArgs)
import Data.List

import Op
import Utils
import ParseBase
import ParseAsm
import CheckArgs

parseOp' candidate args = rightArgsNbr op args && rightTypes op args
  where op = byMnemonic candidate

parseOp candidate args
  | length(args) > 0 = parseOp' candidate (wordsWhen (==',') $ head args)
  | length(args) == 0 = error "No argument given"

parseInstruction' (token:args)
  | head token == '#' = True
  | head token == '.' = parseMetadata token $ intercalate "" args
  | last token == ':' = parseLabel token && ((length(args) > 0 && (parseOp (head args) (tail args))) || True)
  | otherwise = parseOp token args

parseInstruction tokens
  | (length tokens) > 0 = parseInstruction' tokens
  | (length tokens) == 0 = True

worked index line True = putStr ""
worked index line False = putStrLn $ "Syntax error \"" ++ line ++ "\" (line " ++ (show $ index + 1) ++ ")"

-- FIXME : redefine with guards checking for empty list and use tail
parseLine' lines index
  | index < length(lines) = do
      let line = lines !! index in
        worked index line $ parseInstruction $ words line
      parseLine' lines (index + 1)
  | index == length(lines) = putStrLn "Syntax OK"

parseLine lines =
  parseLine' lines 0

parseChampion fileName = do
  championFile <- openFile fileName ReadMode
  content <- hGetContents championFile
  parseLine $ lines content
  hClose championFile

main :: IO ()
main = do
        args <- getArgs
        mapM_ parseChampion args
