import System.IO
import System.Environment (getArgs)
import Data.Char
import Data.List

import Op
import Utils

-- FIXME : add line to every error
-- turn error into printStrLn to avoid stopping parsing

parseString token = (head token == '"') && (last token == '"')

identifierChar c = (((ord(c) >= ord('a')) && (ord(c) <= ord('z'))) || c == '_') || numChar c

numChar c = ((ord(c) >= ord('0')) && (ord(c) <= ord('9')))

parseNum candidate
  | head candidate == '-' = length(filter (not . numChar) $ tail candidate) == 0
  | otherwise = length(filter (not . numChar) candidate) == 0

parseId candidate = length(filter ( not . identifierChar) candidate) == 0

parseMetadata field cstring = (parseId $ tail field) && (parseString cstring)

parseLabel candidate =  parseId $ take (length(candidate) - 1) candidate

-- Check arg types section

isRegister candidate = head candidate == 'r' && parseNum (tail candidate)

isDirect candidate = head candidate == '%' && isIndirect (tail candidate)

isIndirect candidate = isLabel candidate || parseNum candidate

isLabel candidate = head candidate == ':' && parseId (tail candidate)

-- Test argument type
checkArgType' argType arg
  | argType == register = isRegister arg
  | argType == direct = isDirect arg
  | argType == indirect = isIndirect arg

-- Test each authorized types for one argument
checkArgType [argType] arg = checkArgType' argType arg || error ("Argument did not match any authorized type \"" ++ arg ++ "\"")
checkArgType (headArgType:tailArgType) arg = checkArgType' headArgType arg || checkArgType tailArgType arg

-- Test each argument with the set of authorized types
rightTypes' [argTypes] [arg] = checkArgType argTypes arg
rightTypes' (headArgTypes:tailArgTypes) (headArg:tailArg) = checkArgType headArgTypes headArg && rightTypes' tailArgTypes tailArg 

rightTypes op args = rightTypes' argsTypes args
  where argsTypes = getArgsTypes op

-- ENDSECTION

rightArgsNbr op args = (getNbrArgs op == argsNbr
                          || (error $ "Bad # of args for mnemonic \""
                             ++ getMnemonic op
                             ++ "\": "
                             ++ (show argsNbr)
                             ++ " instead of "
                             ++ (show (getNbrArgs op))))
  where argsNbr = length(args)

parseOp' candidate args = rightArgsNbr op args && rightTypes op args
  where op = byMnemonic candidate

parseOp candidate args
  | length(args) > 0 = parseOp' candidate (wordsWhen (==',') $ head args)
  | length(args) == 0 = error "No argument given"

parseInstruction_ (token:args)
  | head token == '#' = True
  | head token == '.' = parseMetadata token $ intercalate "" args
  | last token == ':' = parseLabel token && ((length(args) > 0 && (parseOp (head args) (tail args))) || True)
  | otherwise = parseOp token args

parseInstruction tokens
  | (length tokens) > 0 = parseInstruction_ tokens
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
--  mapM_ parseLine $ lines content
  parseLine $ lines content
  hClose championFile

main :: IO ()
main = do
        args <- getArgs
        mapM_ parseChampion args
