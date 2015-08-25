module CheckArgs (
  rightArgsNbr,
  retrieveTypes
) where

import Op
import ParseBase

-- FIXME : DEBUG
import Debug.Trace

addLabelCall self value = (label, value):self
addRegister self value = (register, value):self
addIndirect self value = (indirect, value):self

-- FIXME : HERE we take the last arg of the list and use its value
-- --> (direct, "label")
-- WHY ARE WE DOING THIS?
-- AND WHY ARE WE STILL GOING IN isDirect EVEN IF WE FOUND a TYPE THAT MATCH?
-- CHECK IF ARGTYPE == 
addDirect self = 
  if argType /= label
  then (direct, value):(init self)
  else self
  where (argType, value) = last self

-- Register : r1 <–> rx with x = REG_NUMBER
-- Example : ld r1,r2 (load r1 in r2)
-- 'r' #identifier
--
-- Direct : The character DIRECT_CHAR followed by a value or a label (preceded
-- by LABEL_CHARS), which represents a direct value.
-- Example : ld $4,r5 (load 4 in r5)
-- Example : ld %:label, r7 (load label in r7)
-- '%' [value | label]
--
-- Indirect : A value or a label (preceded by LABEL_CHARS) which represents the
-- value contained at the address of the parameter, relative to the PC.
-- Example : ld 4,r5 (load the 4 bytes at address (4+PC) in r5).
-- [value | label]

-- Label : id precedeed by LABEL_CHAR

-- FIXME : make a generic function that check if it's a value or a label
--         use it in direct and indirect

-- 'r' #num
isRegister candidate types = (headRes && tailRes, addRegister types value)
  where headRes = (head candidate == 'r')
        (tailRes, value) = parseNum (tail candidate)

-- '%' [label | value]
-- FIXME : should call isLabel?
isDirect candidate types = (headRes && tailRes, addDirect tailTypes)
  where headRes = head candidate == '%' -- && (candidate !! 1) /= ':'
        (tailRes, tailTypes) = isIndirect (tail candidate) types


-- FIXME : how are indirect and label related?
--         can we separate the test completely?
--
--  [label | value]
isIndirect candidate types =
  if (headRes)
  then trace ("LABEL ADDED: " ++ (show headTypes)) (headRes, headTypes)
  else if (tailRes)
       then trace ("NO LABEL FOUND: " ++ (show value)) (tailRes, addIndirect types value)
       else (False, types)
  where (headRes, headTypes) = isLabel candidate types
        (tailRes, value) = parseNum candidate

-- ':' #id
isLabel candidate types = (headRes && tailRes, addLabelCall types value)
  where headRes = head candidate == ':'
        (tailRes, value) = parseId (tail candidate)

-- Test argument type
checkArgType' argType arg types
 | argType == register = isRegister arg types
 | argType == indirect = isIndirect arg types
 | argType == direct = isDirect arg types
checkArgType' _ _ types = (False, types)

-- Test each authorized types for one argument
checkArgType [] _ types = (False, types)
checkArgType [argType] arg types = checkArgType' argType arg types

checkArgType (headArgType:tailArgType) arg types =
  (headRes || tailRes, if tailRes == True then tailTypes else headTypes)
  where
    (headRes, headTypes) = checkArgType' headArgType arg types
    (tailRes, tailTypes) = checkArgType tailArgType arg headTypes

-- Test each argument with the set of authorized types
retrieveTypes' [argTypes] [arg] types = checkArgType argTypes arg types
retrieveTypes' (headArgTypes:tailArgTypes) (headArg:tailArg) types =
  (headRes && tailRes, tailTypes)
  where
    (headRes, headTypes) = checkArgType headArgTypes headArg types
    (tailRes, tailTypes) = retrieveTypes' tailArgTypes tailArg headTypes

retrieveTypes' [] _ types = (False, types)
retrieveTypes' _ [] types = (False, types)

retrieveTypes op args = 
  if validTypes
  then (validTypes, argTypes)
  -- FIXME : add error infos
  else error "Argument did not match any authorized types"
  where (validTypes, argTypes) = retrieveTypes' (getArgsTypes op) args []

rightArgsNbr op args = (getNbrArgs op == argsNbr
                          || (error $ "Bad # of args for mnemonic \""
                             ++ getMnemonic op
                             ++ "\": "
                             ++ (show argsNbr)
                             ++ " instead of "
                             ++ (show (getNbrArgs op))))
  where argsNbr = length(args)
