module Vm.Instructions where

import Foreign.Storable
import Data.Maybe
import Data.Bits
import Data.ByteString.Internal
import qualified Data.ByteString as B

import Op
import Vm.Vm

import Debug.Trace

modIdxMod = flip mod idxMod

-- Followed by 4 bytes representing the player name.
-- This instruction indicates that the player is alive.
-- (No parameter encoding byte)
live :: [Parameter] -> Vm -> Maybe Vm
live [PDirect playerNbr] vm =
  if playerNbr >= 0 && fromIntegral playerNbr < length (programs vm)
  then let program = getCurrentProgram vm
       in Just $ setCurrentProgram (program { alive = True}) vm
  else Nothing 
live _ _ = Nothing

ld_ :: (Int -> Int) -> [Parameter] -> Vm -> Maybe Vm
ld_ f [firstParam, PRegister registerNbr] vm =
  let value = getParameterValue f firstParam vm
      vm' = setCurrentProgramCarry (value /= 0) vm
  in Just $ setCurrentProgramRegister registerNbr (fromIntegral value) vm'
ld_ _ _ _ = Nothing
 
-- This instruction takes 2 parameters,
-- the 2nd of which has to be a register (not the PC)
-- It loads the value of the first parameter in the register.
-- This operation modifies the carry.
--
-- ld 34,r3 loads the REG_SIZE bytes
-- from address (PC + (34 % IDX_MOD)) in register r3.
ld :: [Parameter] -> Vm -> Maybe Vm
ld = ld_ modIdxMod
 
-- This instruction takes 2 parameters.
-- It stores (REG_SIZE bytes) the value of the first argument (always a register)
-- in the second.
-- st r4,34 stores the value of r4 at the address (PC + (34 % IDX_MOD))
-- st r3,r8 copies r3 in r8
st :: [Parameter] -> Vm -> Maybe Vm
st [PRegister registerNbr, secondParam] vm =
  let value = getCurrentProgramRegister registerNbr vm
  in case secondParam of
   PRegister dstPRegister -> Just $ setCurrentProgramRegister dstPRegister value vm
   PIndirect offset -> Just $ setMemoryByCurrentProgramPc modIdxMod (fromIntegral offset) (fromIntegral value) regSize vm
st _ _ = Nothing

registerOperation :: (RegisterValue -> RegisterValue -> RegisterValue) -> [Parameter] -> Vm -> Maybe Vm
registerOperation f [PRegister r1, PRegister r2, PRegister r3] vm = 
  let r1Value = getCurrentProgramRegister r1 vm
      r2Value = getCurrentProgramRegister r2 vm
      result = f r1Value r2Value
  in Just $ setCurrentProgramRegister r3 result vm
registerOperation _ _ _ = Nothing

registerCarryOperation :: (Int -> Int -> Int) -> [Parameter] -> Vm -> Maybe Vm
registerCarryOperation f [firstParam, secondParam, PRegister registerNbr] vm =
  let firstValue = getParameterValue id firstParam vm
      secondValue = getParameterValue id secondParam vm
      result = f firstValue secondValue
      vm' = setCurrentProgramRegister registerNbr (fromIntegral result) vm
  in Just $ setCurrentProgramCarry (result /= 0) vm'
registerCarryOperation _ params _ = Nothing

-- This instruction takes 3 registers as parameter,
-- adds the contents of the 2 first and stores the result in the third.
-- This operation modifies the carry.
--
-- add r2,r3,r5 adds r2 and r3 and stores the result in r5
add = registerOperation (+)

-- Same effect as add, but with a substraction
sub = registerOperation (-)

-- p1 & p2 -> p3, the parameter 3 is always a register
-- This operation modifies the carry.
--
-- and r2, %0,r3 stores r2 & 0 in r3.
and = registerCarryOperation (.&.)

-- Same as and but with an or (| in C)
or = registerCarryOperation (.|.)

-- Same as and but with an or (^ in C)
xor = registerCarryOperation Data.Bits.xor
 
-- This instruction is not followed by any parameter encoding byte.
-- It always takes an index (IND_SIZE) 
-- if the carry is set to 1 makes a jump at this index
-- If the carry is null zjmp does nothing but consumes the same amount of time
--
-- zjmp %23 does : If carry == 1, store (PC + (23 % IDX_MOD)) in the PC.
zjmp :: [Parameter] -> Vm -> Maybe Vm
zjmp [PDirect offset] vm =
  let currentProgram = getCurrentProgram vm
--      offset' = modIdxMod $ fromIntegral $ if offset > 32767 then (65535 - offset) * (-1) else offset
  in if carry currentProgram
     then Just $ setCurrentProgramPc (pc currentProgram + fromIntegral offset) vm
     else Just vm
zjmp _ _ = Nothing
 
ldi_ :: (Int -> Int) -> [Parameter] -> Vm -> Maybe Vm
ldi_ f [firstParam, secondParam, PRegister registerNbr] vm =
  let firstValue = getParameterValue f firstParam vm
      s = PIndirect $ fromIntegral $ firstValue + parameterValue secondParam
      finalValue = getParameterValue f s vm
      vm' = setCurrentProgramCarry (finalValue /= 0) vm
  in Just $ setCurrentProgramRegister registerNbr (fromIntegral finalValue) vm
ldi_ _ _ _ = Nothing

-- This operation modifies the carry.
-- ldi 3,%4,r1 reads IND_SIZE bytes at address: (PC + (3 % IDX_MOD)),
-- adds 4 to this value.
-- We will name this sum S.
-- Read REG_SIZE bytes at address (PC + (S % IDX_MOD)),
-- which are copied to r1.
-- Parameters 1 and 2 are indexes.
ldi :: [Parameter] -> Vm -> Maybe Vm
ldi = ldi_ modIdxMod

--         is this all good?
--         how does it compare to st?
-- sti r2,%4,%5 sti
-- copies REG_SIZE bytes of r2 at address (4 + 5)
-- Parameters 2 and 3 are indexes.
-- If they are, in fact, registers, we’ll use their contents as indexes.

-- FIXME : do we apply idxMod?
sti :: [Parameter] -> Vm -> Maybe Vm
sti [PRegister registerNbr, secondParam, thirdParam] vm =
  let toStore = getCurrentProgramRegister registerNbr vm
      secondParamValue = getParameterValue id secondParam vm
      thirdParamValue = getParameterValue id thirdParam vm

      offset = fromIntegral $ secondParamValue + thirdParamValue
  in Just $ setMemoryByCurrentProgramPc id offset (fromIntegral toStore) regSize vm
sti _ _ = Nothing

fork_ :: (Int -> Int) -> [Parameter] -> Vm -> Maybe Vm
fork_ f [param] vm =
  let program = getCurrentProgram vm
      offset = parameterValue param
      pc' = f $ fromIntegral $ offset + pc program
      program' = program { pc = fromIntegral pc' :: Offset }
  in Just $ vm { programs = programs vm ++ [program'] }
fork_ _ _ _ = Nothing

-- This instruction is not followed by a parameter encoding byte.
-- It always takes an index and creates a new program,
-- which is executed from address : (PC + (first parameter % IDX_MOD)).
--
-- Fork %34 creates a new program.
-- The new program inherits all of its father’s states.
fork :: [Parameter] -> Vm -> Maybe Vm
fork = fork_ modIdxMod
 
-- Same as ld, but without the % IDX_MOD This operation modifies the carry.
lld :: [Parameter] -> Vm -> Maybe Vm
lld = ld_ id
 
-- Same as ldi, without the % IDX_MOD This operation modifies the carry.
lldi :: [Parameter] -> Vm -> Maybe Vm
lldi = ldi_ id
 
-- Same as fork, without the % IDX_MOD This operation modifies the carry.
lfork :: [Parameter] -> Vm -> Maybe Vm
lfork = fork_ id
 
-- This instruction is followed by a parameter encoding byte.
-- It takes a register and displays the character the ASCII code contained in it.
-- (a modulo 256 is applied to this ascii code, the char is displayed on stdout)
--
-- Ex: ld %52,r3
-- aff r3
-- Displays ’*’ on the standard output
aff :: [Parameter] -> Vm -> Maybe Vm
aff [PRegister registerNbr] vm =
  let value = getCurrentProgramRegister registerNbr vm
      aff' = affBuffer vm ++ [w2c value]
  in Just $ vm { affBuffer = aff' }
aff _ _ = Nothing

instructionTable = [
  live,
  ld,
  st,
  add,
  sub,
  Vm.Instructions.and,
  Vm.Instructions.or,
  Vm.Instructions.xor,
  zjmp,
  ldi,
  sti,
  fork,
  lld,
  lldi,
  lfork,
  aff]
