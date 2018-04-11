module Asm.Parsing.CheckArgs where

import Data.Maybe
import Data.Either

import Op
import Asm.ChampionData
import Asm.Parsing.ParseBase

import Utils

type ArgTypeAccumulator = [Parameter]
type CheckResult = Maybe Parameter
type CheckArgTypeResult = (String, ArgTypeAccumulator)

-- Register : r1 <–> rx with x = REG_NUMBER
-- Example : ld r1,r2 (load r1 in r2)
-- 'r' #num
isRegister :: ArgContent -> CheckResult
isRegister candidate =
  let (c:cs) = candidate
      registerNumber = parseNum cs
  in if c == 'r' && not (null cs) && isJust registerNumber
     then fmap Register registerNumber
     else Nothing

-- Label : id precedeed by LABEL_CHAR
-- ':' #id
isLabel :: ArgContent -> CheckResult
isLabel candidate =
  let (c:cs) = candidate
      labelName = parseId cs
  in if c == ':' && isJust labelName
     then fmap Label labelName
     else Nothing

getArgValue :: ArgType ArgContent -> ArgContent
getArgValue (Label value) = value

argIsLabel :: Maybe (ArgType ArgContent) -> Bool
argIsLabel (Just (Label _)) = True
argIsLabel _                = False

-- Direct : The character DIRECT_CHAR followed by a value or a label,
-- which represents a direct value.
-- Example : ld $4,r5 (load 4 in r5)
-- Example : ld %:label, r7 (load label in r7)
-- '%' [ label | value ]
isDirect :: ArgContent -> CheckResult
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
isIndirect :: ArgContent -> CheckResult
isIndirect candidate =
  let labelResult = isLabel candidate
      indirectRes = parseNum candidate
  in if isJust labelResult
     then labelResult
     else fmap Indirect indirectRes

-- checkArgTypes

-- Test argument type
checkArgType' :: ArgContent -> ArgTypeAccumulator -> ArgType () -> ArgTypeAccumulator
checkArgType' arg typesAcc argType  =
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
checkArgType :: Int -> ChampionData -> CheckArgTypeResult -> ([ArgType ()], ArgContent) -> CheckArgTypeResult
checkArgType opArgsNbr championData (errors, previousAcc) (argTypes, arg) =
  let endResult = foldl (checkArgType' arg) previousAcc argTypes
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
checkArgTypes :: Op -> [ArgContent] -> ChampionData -> Either String ArgTypeAccumulator
checkArgTypes op args championData =
  let opArgsTypes = getArgsTypes op -- get valid types from op
      opArgsNbr = getNbrArgs op
      argsNbr = length args
-- FIXME : name shadowing
      checkArgType' = checkArgType opArgsNbr championData
      instructionsToCheck = zip opArgsTypes args
  in if argsNbr == opArgsNbr
     then let (errors, types) = foldl checkArgType' ("", []) instructionsToCheck
          in if not (null errors)
             then Left errors
             else Right types
     else Left $ argNumberError op argsNbr opArgsNbr championData
