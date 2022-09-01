module Vm.Instructions where

import Foreign.Storable
import Data.Word
import Data.Int
import Data.Bits
import Data.ByteString.Internal
import qualified Data.ByteString as B

import Op
import Vm.Vm

import Utils

modIdxMod = flip mod idxMod
modNegIdxMod = flip mod (idxMod * (-1))

getParameterValueId = getParameterValue id

type InstructionFn = [Parameter] -> Vm -> Vm

-- alive
-- Followed by 4 bytes representing the player name.
-- This instruction indicates that the player is alive.
-- (No parameter encoding byte)
live :: [Parameter] -> Vm -> Vm
live [Direct championNbr] vm =
  let championIndex = fromIntegral championNbr - 1
      program = getProgramAt championIndex vm
  in setProgramAt (program { alive = True }) championIndex vm

ld_ :: (Int -> Int) -> [Parameter] -> Vm -> Vm
ld_ f [firstParam, Register registerNbr] vm =
  let value = getParameterValue f firstParam regSize vm
      vm' = setCurrentProgramCarry (value /= 0) vm
  in setCurrentProgramRegister registerNbr (fromIntegral value) vm'
 
-- load
-- This instruction takes 2 parameters,
-- the 2nd of which has to be a register (not the PC)
-- It loads the value of the first parameter in the register.
-- This operation modifies the carry.
--
-- ld 34,r3 loads the REG_SIZE bytes
-- from address (PC + (34 % IDX_MOD)) in register r3.
ld :: [Parameter] -> Vm -> Vm
ld = ld_ modIdxMod
 
-- store
-- This instruction takes 2 parameters.
-- It stores (REG_SIZE bytes) the value of the first argument (always a register)
-- in the second.
-- st r4,34 stores the value of r4 at the address (PC + (34 % IDX_MOD))
-- st r3,r8 copies r3 in r8
st :: [Parameter] -> Vm -> Vm
st [Register registerNbr, secondParam] vm =
  let value = getCurrentProgramRegister registerNbr vm
  in case secondParam of
   Register dstRegister -> setCurrentProgramRegister dstRegister value vm
   Indirect offset -> setMemoryByCurrentProgramPc modIdxMod (fromIntegral offset) (fromIntegral value) regSize vm

registerOperation :: (RegisterValue -> RegisterValue -> RegisterValue) -> [Parameter] -> Vm -> Vm
registerOperation f [Register r1, Register r2, Register r3] vm = 
  let r1Value = getCurrentProgramRegister r1 vm
      r2Value = getCurrentProgramRegister r2 vm
      result = f r1Value r2Value
  in setCurrentProgramRegister r3 result vm

registerCarryOperation :: (Int -> Int -> Int) -> [Parameter] -> Vm -> Vm
registerCarryOperation f [firstParam, secondParam, Register registerNbr] vm =
  let firstValue = getParameterValueId firstParam regSize vm -- size is for the indirect only
      secondValue = getParameterValueId secondParam regSize vm -- size is for the indirect only
      result = f (fromIntegral firstValue) (fromIntegral secondValue)
      vm' = setCurrentProgramRegister registerNbr (fromIntegral result) vm
  in setCurrentProgramCarry (result /= 0) vm'

-- addition
-- This instruction takes 3 registers as parameter,
-- adds the contents of the 2 first and stores the result in the third.
-- This operation modifies the carry.
--
-- add r2,r3,r5 adds r2 and r3 and stores the result in r5
add = registerOperation (+)

-- substraction
-- Same effect as add, but with a substraction
sub = registerOperation (-)

-- bin and
-- p1 & p2 -> p3, the parameter 3 is always a register
-- This operation modifies the carry.
--
-- and r2, %0,r3 stores r2 & 0 in r3.
and = registerCarryOperation (.&.)

-- bin or
-- Same as and but with an or (| in C)
or = registerCarryOperation (.|.)

-- bin xor
-- Same as and but with an or (^ in C)
xor = registerCarryOperation Data.Bits.xor
 
maxUShort = maxBound :: Word16 
maxShort = maxBound :: Int16

-- FIXME : put mod back
signedJmp :: Word16 -> Int16 -> Word16
signedJmp origin offset =
  if offset < maxShort
--  then origin + (fromIntegral (modIdxMod (fromIntegral offset)))                                   -- > 0
--  else origin - (fromIntegral (modNegIdxMod (fromIntegral (maxUShort - fromIntegral offset) + 1))) -- < 0
  then origin + (fromIntegral (fromIntegral offset))                                   -- > 0
  else origin - (fromIntegral (fromIntegral (maxUShort - fromIntegral offset) + 1)) -- < 0

-- jump if zero
-- This instruction is not followed by any parameter encoding byte.
-- It always takes an index (IND_SIZE) 
-- if the carry is set to 1 makes a jump at this index
-- If the carry is null zjmp does nothing but consumes the same amount of time
--
-- zjmp %23 does : If carry == 1, store (PC + (23 % IDX_MOD)) in the PC.
zjmp :: [Parameter] -> Vm -> Vm
zjmp [Direct offset] vm =
  let currentProgram = getCurrentProgram vm
      newPc = fromIntegral $ signedJmp (fromIntegral $ pc currentProgram) (fromIntegral offset)
  in if carry currentProgram
     then vm
     else setCurrentProgramPc (fromIntegral newPc) vm
 
ldi_ :: (Int -> Int) -> [Parameter] -> Vm -> Vm
ldi_ f [firstParam, secondParam, Register registerNbr] vm =
  let firstValue = getParameterValue f firstParam regSize vm -- size is for the indirect only
      secondValue = getParameterValue f secondParam regSize vm
      s = Indirect $ fromIntegral $ firstValue + secondValue
      finalValue = getParameterValue f s regSize vm
      vm' = setCurrentProgramCarry (finalValue /= 0) vm
  in setCurrentProgramRegister registerNbr (fromIntegral finalValue) vm

-- load index
-- This operation modifies the carry.
-- ldi 3,%4,r1 reads IND_SIZE bytes at address: (PC + (3 % IDX_MOD)),
-- adds 4 to this value.
-- We will name this sum S.
-- Read REG_SIZE bytes at address (PC + (S % IDX_MOD)),
-- which are copied to r1.
-- Parameters 1 and 2 are indexes.
ldi :: [Parameter] -> Vm -> Vm
ldi = ldi_ modIdxMod

-- FIXME : is this all good?
--         how does it compare to st?
-- FIXME : do we apply idxMod?

-- store index
-- sti r2,%4,%5
-- sti copies REG_SIZE bytes of r2 at address (4 + 5)
-- Parameters 2 and 3 are indexes.
-- If they are, in fact, registers, we’ll use their contents as indexes. -- FIXME
sti :: [Parameter] -> Vm -> Vm
sti [Register registerNbr, secondParam, thirdParam] vm =
  let toStore = getCurrentProgramRegister registerNbr vm
      secondParamValue = getParameterValueId secondParam regSize vm
      thirdParamValue = getParameterValueId thirdParam regSize vm

      offset = fromIntegral $ secondParamValue + thirdParamValue
  in setMemoryByCurrentProgramPc id offset (fromIntegral toStore) regSize vm

fork_ :: (Int -> Int) -> [Parameter] -> Vm -> Vm
fork_ f [param] vm =
  let program = getCurrentProgram vm
      offset = getParameterValue f param regSize vm -- FIXME : what size, what f
      pc' = f $ fromIntegral $ offset + pc program
      program' = program { pc = fromIntegral pc' :: Offset }
  in vm { programs = programs vm ++ [program'] }

-- fork
-- This instruction is not followed by a parameter encoding byte.
-- It always takes an index and creates a new program,
-- which is executed from address : (PC + (first parameter % IDX_MOD)).
--
-- fork %34 creates a new program.
-- The new program inherits all of its father’s states.
fork :: [Parameter] -> Vm -> Vm
fork = fork_ modIdxMod

-- long load
-- Same as ld, but without the % IDX_MOD This operation modifies the carry.
lld :: [Parameter] -> Vm -> Vm
lld = ld_ id
 
-- long load index
-- Same as ldi, without the % IDX_MOD This operation modifies the carry.
lldi :: [Parameter] -> Vm -> Vm
lldi = ldi_ id
 
-- long fork
-- Same as fork, without the % IDX_MOD This operation modifies the carry.
lfork :: [Parameter] -> Vm -> Vm
lfork = fork_ id
 
-- FIXME : move buffer to program
-- aff
-- This instruction is followed by a parameter encoding byte.
-- It takes a register and displays the character the ASCII code contained in it.
-- (a modulo 256 is applied to this ascii code, the char is displayed on stdout)
--
-- Ex: ld %52,r3
-- aff r3
-- Displays ’*’ on the standard output
aff :: [Parameter] -> Vm -> Vm
aff [Register registerNbr] vm =
  let value = getCurrentProgramRegister registerNbr vm
      aff' = affBuffer vm ++ [w2c value]
  in vm { affBuffer = aff' }

setInstrutionNameWrapper :: Int -> InstructionFn -> [Parameter] -> Vm -> Vm
setInstrutionNameWrapper index f parameters vm =
  let instructionName = opsNames !! index
      vm' = setCurrentProgramInstruction instructionName vm
  in f parameters vm'

wrapInstructionWithSetName :: [InstructionFn] -> [InstructionFn]
wrapInstructionWithSetName table = 
  mapWithIndex setInstrutionNameWrapper table

instructionTable = wrapInstructionWithSetName([
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
  aff])
