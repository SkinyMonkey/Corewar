module CheckArgs (
  rightArgsNbr,
  checkArgTypes
) where

import Op
import ParseBase
import ChampionData

-- FIXME : DEBUG
import Debug.Trace

-- ArgType : ArgType, indirect, direct, label
-- ArgContent : Token, value
type ArgContent = String
type ArgTypeAccumulator = [(ArgType, ArgContent)]
type CheckResult = (Bool, ArgTypeAccumulator)

addLabelCall :: ArgTypeAccumulator -> ArgContent -> ArgTypeAccumulator
addLabelCall self labelName = (Label, labelName):self

addRegister :: ArgTypeAccumulator -> ArgContent -> ArgTypeAccumulator
addRegister self registerNumber = (Register, registerNumber):self

addIndirect :: ArgTypeAccumulator -> ArgContent -> ArgTypeAccumulator
addIndirect self indirectValue = (Indirect, indirectValue):self

-- FIXME : HERE we take the last arg of the list and use its value
-- --> (direct, "label")
-- WHY ARE WE DOING THIS?
-- AND WHY ARE WE STILL GOING IN isDirect EVEN IF WE FOUND a TYPE THAT MATCH?
-- CHECK IF ARGTYPE == 
addDirect :: ArgTypeAccumulator -> ArgTypeAccumulator
addDirect self = 
  if argType /= Label
  then (Direct, value):(init self)
  else self
  where (argType, value) = last self

-- FIXME : make a generic function that check if it's a value or a label
--         use it in direct and indirect

-- Register : r1 <–> rx with x = REG_NUMBER
-- Example : ld r1,r2 (load r1 in r2)
-- 'r' #num
isRegister :: ArgContent -> ArgTypeAccumulator -> CheckResult
isRegister candidate typesAcc =
  let registerChar = head candidate
      registerNumber = tail candidate
  in if registerChar == 'r' && (solved $ parseNum registerNumber)
     then resolve $ addRegister typesAcc registerNumber
     else reject typesAcc

-- Direct : The character DIRECT_CHAR followed by a value or a label (preceded
-- by LABEL_CHARS), which represents a direct value.
-- Example : ld $4,r5 (load 4 in r5)
-- Example : ld %:label, r7 (load label in r7)
-- '%' [ label | value ]
-- FIXME : should call isLabel?
isDirect :: ArgContent -> ArgTypeAccumulator -> CheckResult
isDirect candidate typesAcc =
  let directChar = head candidate
      indirectValue = tail candidate
      (indirectRes, indirectTypes) = isIndirect indirectValue typesAcc
  in if directChar == '%' && indirectRes -- || label || value? -- && (candidate !! 1) /= ':'
     then resolve $ addDirect indirectTypes
     else reject typesAcc

-- FIXME : how are indirect and label related?
--         can we separate the test completely?

-- Indirect : A value or a label which represents the
-- value contained at the address of the parameter, relative to the PC.
-- Example : ld 4,r5 (load the 4 bytes at address (4+PC) in r5).
-- [value | label]
isIndirect :: ArgContent -> ArgTypeAccumulator -> CheckResult
isIndirect candidate typesAcc =
  let labelResult = isLabel candidate typesAcc
  in if solved labelResult
     then labelResult
     else let (indirectRes, value) = parseNum candidate
          in if indirectRes
             then resolve $ addIndirect typesAcc value
             else reject typesAcc

-- Label : id precedeed by LABEL_CHAR
-- ':' #id
isLabel :: ArgContent -> ArgTypeAccumulator -> CheckResult
isLabel candidate typesAcc =
  let labelChar = head candidate
      labelName = tail candidate
  in if labelChar == ':' && (solved $ parseId labelName)
     then resolve $ addLabelCall typesAcc labelName
     else reject typesAcc

-- checkArgTypes

-- Test argument type
checkArgType' :: ArgContent -> ArgType -> CheckResult -> CheckResult
checkArgType' arg argType result =
 let (_, typesAcc) = result
     currentResult = case argType of
      Register -> isRegister arg typesAcc
      Indirect -> isIndirect arg typesAcc
      Direct -> isDirect arg typesAcc
     in if solved currentResult
        then currentResult
        else result

-- FIXME : replace foldr by foldl, no reason to use foldr

-- Test each authorized types for one argument
checkArgType :: ([ArgType], ArgContent) -> CheckResult -> CheckResult
-- FIXME : CHECK that it's effectively a OR and not an AND or worse
checkArgType (argTypes, arg) result =
  let currentResult = foldr (checkArgType' arg) result argTypes
  in if solved currentResult
     then currentResult
     else result

-- Returns an evaluation of the res
checkArgTypes :: Op -> [ArgContent] -> CheckResult
checkArgTypes op args = 
  let opArgsTypes = getArgsTypes op -- get valid types from op
      result = resolve ([])
      endResult = foldr checkArgType result $ zip opArgsTypes args
  in if solved endResult
     then endResult
     -- FIXME : add error infos
     else error "Argument did not match any authorized types"

-- END

rightArgsNbr :: Op -> [ArgContent] -> ChampionData -> Bool
rightArgsNbr op args championData = (getNbrArgs op == argsNbr
                          || (error $ "Bad # of args for mnemonic \""
                             ++ getMnemonic op
                             ++ "\": "
                             ++ (show argsNbr)
                             ++ " instead of "
                             ++ (show (getNbrArgs op))
                             ++ " in \""
                             ++ getCurrentLine championData
                             ++ "\""
                             ))
  where argsNbr = length(args)
