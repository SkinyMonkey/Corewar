module CheckArgs where

import Data.Maybe
import Data.Either

import Op
import ParseBase
import ChampionData

import Utils

type ArgContent = String
type ArgTypeAccumulator = [ArgType ArgContent]
type CheckResult = Maybe ArgTypeAccumulator
type CheckArgTypeResult = (String, ArgTypeAccumulator)

-- Register : r1 <–> rx with x = REG_NUMBER
-- Example : ld r1,r2 (load r1 in r2)
-- 'r' #num
isRegister :: ArgContent -> ArgTypeAccumulator -> CheckResult
isRegister candidate typesAcc =
  let (c:cs) = candidate
      registerNumber = parseNum cs
  in if c == 'r' && not (null cs) && msolved registerNumber
     then Just $ typesAcc ++ [Register (fromJust registerNumber)]
     else Nothing

-- Label : id precedeed by LABEL_CHAR
-- ':' #id
isLabel :: ArgContent -> ArgTypeAccumulator -> CheckResult
isLabel candidate typesAcc =
  let (c:cs) = candidate
      labelName = parseId cs
  in if c == ':' && msolved labelName
     then Just $ typesAcc ++ [Label (fromJust labelName)]
     else Nothing

getArgValue :: ArgType ArgContent -> ArgContent
getArgValue (Label value) = value

argIsLabel :: ArgType ArgContent -> Bool
argIsLabel (Label _) = True
argIsLabel _         = False

-- Direct : The character DIRECT_CHAR followed by a value or a label,
-- which represents a direct value.
-- Example : ld $4,r5 (load 4 in r5)
-- Example : ld %:label, r7 (load label in r7)
-- '%' [ label | value ]
isDirect :: ArgContent -> ArgTypeAccumulator -> CheckResult
isDirect candidate typesAcc =
  let (c:cs) = candidate
      result = isIndirect cs []
      directValue = parseNum cs
  in if c == '%'
     then if isJust result && argIsLabel (last $ fromJust result)  -- We get the evaluated label
          then Just $ typesAcc ++ [last $ fromJust result]
          else if msolved directValue -- direct value
               then Just $ typesAcc ++ [Direct (fromJust directValue)]
               else Nothing
     else Nothing

-- Indirect : A value or a label which represents the
-- value contained at the address of the parameter, relative to the PC.
-- Example : ld 4,r5 (load the 4 bytes at address (4+PC) in r5).
-- [ label | value ]
isIndirect :: ArgContent -> ArgTypeAccumulator -> CheckResult
isIndirect candidate typesAcc =
  let labelResult = isLabel candidate typesAcc
      indirectRes = parseNum candidate
  in if msolved labelResult
     then labelResult
     else if msolved indirectRes
          then Just $ typesAcc ++ [Indirect (fromJust indirectRes)]
          else Nothing

-- checkArgTypes

-- Test argument type
checkArgType' :: ArgContent -> ArgType () -> ArgTypeAccumulator -> ArgTypeAccumulator
checkArgType' arg argType typesAcc =
 let currentResult = case argType of
      Register _ -> isRegister arg typesAcc
      Indirect _ -> isIndirect arg typesAcc
      Direct _ -> isDirect arg typesAcc
      Label _ -> undefined -- should never happen
     in if msolved currentResult
        then fromJust currentResult
        else typesAcc

-- FIXME : replace foldr by foldl, no reason to use foldr

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
checkArgType :: Int -> ChampionData -> ([ArgType ()], ArgContent) -> CheckArgTypeResult -> CheckArgTypeResult
checkArgType opArgsNbr championData (argTypes, arg) (errors, previousAcc) =
  let endResult = foldr (checkArgType' arg) previousAcc argTypes
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
      checkArgType' = checkArgType opArgsNbr championData
  in if argsNbr == opArgsNbr
     then let (errors, types) = foldr checkArgType' ("", []) $ zip opArgsTypes args
          in if not (null errors)
             then Left errors
             else Right types
     else Left $ argNumberError op argsNbr opArgsNbr championData
