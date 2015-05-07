module CheckArgs (
  rightArgsNbr,
  retrieveTypes
) where

import Op
import ParseBase

addLabelCall self value = (label, value):self
addRegister self value = (register, value):self
addIndirect self value = (indirect, value):self
addDirect self = (direct, snd lastPair):(init self)
  where lastPair = last self

isRegister candidate types = (headRes && tailRes, addRegister types value)
  where headRes = (head candidate == 'r')
        (tailRes, value) = parseNum (tail candidate)

isDirect candidate types = (headRes && tailRes, addDirect tailTypes)
  where headRes = head candidate == '%'
        (tailRes, tailTypes) = isIndirect (tail candidate) types

isIndirect candidate types =
  if (headRes)
  then (headRes, headTypes)
  else if (tailRes)
       then (tailRes, addIndirect types value)
       else (False, types)
  where (headRes, headTypes) = isLabel candidate types
        (tailRes, value) = parseNum candidate

isLabel candidate types = (headRes && tailRes, addLabelCall types value)
  where headRes = head candidate == ':'
        (tailRes, value) = parseId (tail candidate)

-- Test argument type
checkArgType' argType arg types
  | argType == register = isRegister arg types
  | argType == direct = isDirect arg types
  | argType == indirect = isIndirect arg types
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
