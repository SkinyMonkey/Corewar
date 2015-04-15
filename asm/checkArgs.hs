module CheckArgs (
  rightArgsNbr,
  rightTypes
) where

import Op
import ParseBase

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

rightArgsNbr op args = (getNbrArgs op == argsNbr
                          || (error $ "Bad # of args for mnemonic \""
                             ++ getMnemonic op
                             ++ "\": "
                             ++ (show argsNbr)
                             ++ " instead of "
                             ++ (show (getNbrArgs op))))
  where argsNbr = length(args)