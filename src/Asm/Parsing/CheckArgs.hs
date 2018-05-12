module Asm.Parsing.CheckArgs where

import Data.Maybe
import Data.Either

import Op
import Asm.ChampionData
import Asm.Parsing.ParseBase

import Utils

type ParameterAccumulator = [Parameter]
type CheckParameterResult = (String, ParameterAccumulator)

-- Register : r1 <–> rx with x = REG_NUMBER
-- Example : ld r1,r2 (load r1 in r2)
-- 'r' #num
isRegister :: String -> Maybe Parameter
isRegister candidate =
  let (c:cs) = candidate
      registerNumber = parseNum cs
  in if c == 'r' && not (null cs) && isJust registerNumber
     then fmap (Register . fromIntegral) registerNumber
     else Nothing

-- Label : id precedeed by LABEL_CHAR
-- ':' #id
isLabel :: String -> Maybe Parameter
isLabel candidate =
  let (c:cs) = candidate
      labelName = parseId cs
  in if c == ':' && isJust labelName
     then fmap Label labelName
     else Nothing

getArgValue :: Parameter -> String
getArgValue (Label value) = value

argIsLabel :: Maybe Parameter -> Bool
argIsLabel (Just (Label _)) = True
argIsLabel _                = False

-- Direct : The character DIRECT_CHAR followed by a value or a label,
-- which represents a direct value.
-- Example : ld $4,r5 (load 4 in r5)
-- Example : ld %:label, r7 (load label in r7)
-- '%' [ label | value ]
isDirect :: String -> Maybe Parameter
isDirect candidate =
  let (c:cs) = candidate
      result = isIndirect cs
      directValue = parseNum cs
  in if c == '%'
     then if isJust result && argIsLabel result  -- We get the evaluated label
          then result
          else fmap Direct directValue -- direct value
     else Nothing

-- Indirect : A value or a label which represents the
-- value contained at the address of the parameter, relative to the PC.
-- Example : ld 4,r5 (load the 4 bytes at address (4+PC) in r5).
-- [ label | value ]
isIndirect :: String -> Maybe Parameter
isIndirect candidate =
  let labelResult = isLabel candidate
      indirectRes = parseNum candidate
  in if isJust labelResult
     then labelResult
     else fmap (Indirect . fromIntegral) indirectRes

-- checkParameters

-- Test argument type
checkParameter' :: String -> ParameterAccumulator -> Parameter -> ParameterAccumulator
checkParameter' arg typesAcc argType  =
 let currentResult = case argType of
      Register _ -> isRegister arg
      Indirect _ -> isIndirect arg
      Direct _ -> isDirect arg
      Label _ -> undefined -- should never happen
     in if isJust currentResult
        then typesAcc ++ [fromJust currentResult]
        else typesAcc

noMatchingTypeError championData arg argTypes endResult previousAcc = concat [
  "Argument did not match any authorized types :\ntoken : ",
  show arg,
  " -> valid possible argTypes ",
  show argTypes,
  " -> found argType ",
  show endResult,
  " -> result ",
  show previousAcc,
  " at line ",
  show $ getLineNbr championData
  ]

-- Test each authorized types for one argument
-- Accumulates types and errors
checkParameter :: Int -> ChampionData -> CheckParameterResult -> ([Parameter], String) -> CheckParameterResult
checkParameter opArgsNbr championData (errors, previousAcc) (argTypes, arg) =
  let endResult = foldl (checkParameter' arg) previousAcc argTypes
  in if length endResult > length previousAcc
     then (errors, endResult)
     else let newError = noMatchingTypeError championData arg argTypes endResult previousAcc
          in ( errors ++ "\n" ++ newError, previousAcc)

argNumberError op argsNbr opArgsNbr championData = concat [
  "Bad # of args for mnemonic \"",
  getMnemonic op,
  "\": ",
  show argsNbr,
  " instead of ",
  show opArgsNbr,
  " in \"",
  getCurrentLine championData,
  "\"",
  " at line ",
  show $ getLineNbr championData
  ]

-- Returns an evaluation of the res
checkParameters :: Op -> [String] -> ChampionData -> Either String ParameterAccumulator
checkParameters op args championData =
  let opArgsTypes = getArgsTypes op -- get valid types from op
      opArgsNbr = getNbrArgs op
      argsNbr = length args
-- FIXME : name shadowing
      checkParameter' = checkParameter opArgsNbr championData
      instructionsToCheck = zip opArgsTypes args
  in if argsNbr == opArgsNbr
     then let (errors, types) = foldl checkParameter' ("", []) instructionsToCheck
          in if not (null errors)
             then Left errors
             else Right types
     else Left $ argNumberError op argsNbr opArgsNbr championData
