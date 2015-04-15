import System.IO
import System.Environment (getArgs)
import Data.List

import Data.Bits

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

-- FIXME : add encoding based on arg type
encodeDescription (p1:p2:p3:p4:[]) = "p4"
encodeDescription (p1:p2:p3:[]) = "p3"
encodeDescription (p1:p2:[]) = "p2"
encodeDescription (p1:[]) = "p1"
encodeDescription [] = "no p"

generateMetadata token args = putStrLn $ "Field : " ++ token ++ " Value : " ++ args

generateLabel token args = putStr ""

generateOp token args = putStrLn $ token
                        ++ " "
                        ++ show args
                        ++ " "
                        ++ encodeDescription (wordsWhen (==',') $ head args)
--  | token == "live" || token == "zjmp" || token == "fork" || token == "lfork"
--    = putStrLn $ token ++ " " ++ (show $ getCode op)
--  | otherwise = putStrLn $ token ++ " " ++ encodeDescription args
  where op = byMnemonic token

generateInstruction [] = putStr ""
generateInstruction (token:args)
  | head token == '#' = putStr ""
  | head token == '.' = generateMetadata token $ intercalate "" args
  | last token == ':' = do
--      generateLabel token -- FIXME : needed?
      if length(args) > 0
      then generateOp (head args) (tail args)
      else putStr ""
  | otherwise = generateOp token args

generateCode' lines index
  | index < length(lines) = do
      let line = lines !! index in
        generateInstruction (words line)
      generateCode' lines (index + 1)
  | index == length(lines) = do
      putStrLn "Code generated"

generateCode lines = do
      generateCode' lines 0

-- FIXME : redefine with guards checking for empty list and use tail
parseLine' lines index
  | index < length(lines) = do
      let line = lines !! index in
        worked index line $ parseInstruction $ words line
      parseLine' lines (index + 1)
  | index == length(lines) = do
      putStrLn "Syntax OK"
      generateCode lines

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
