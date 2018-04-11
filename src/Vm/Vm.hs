module Vm.Vm where

import Data.Word
import Control.Lens
import qualified Data.ByteString as B

import Op

type RegisterNbr = Word8
type RegisterValue = Word8

type ChampionNbr = Word32

-- FIXME : reuse ArgType
data Parameter = PRegister Word8 | PDirect Word32 | PIndirect Word16 deriving (Show, Eq)

data Instruction = Instruction {
  instruction :: Word8,
  params :: [Parameter]
} deriving (Show, Eq)

data Program = Program {
  number :: Int,
  name :: String,
  registers :: [Word32],
  pc :: Offset, -- eip
  carry :: Bool,
  alive :: Bool,
  cycleLeft :: Int, -- before next instruction
  instructions :: B.ByteString
} deriving (Show, Eq)

newProgram :: Int -> String -> Offset -> B.ByteString -> Program
newProgram number name pcOffset instructions = Program {
  number = number,
  name = name,
  registers = replicate regNumber 0,
  pc = pcOffset,
  carry = False,
  alive = True,
  cycleLeft = 0,
  instructions = instructions
}

data Vm = Vm {
  programs :: [Program],
  currentProgramNbr :: Int,
  memory :: [Word8],
  affBuffer :: String
} deriving (Show, Eq)

newVm :: [Program] -> Vm
newVm programs = Vm {
  programs = programs,
  currentProgramNbr = 0,
  memory = replicate memSize 0,
  affBuffer = ""
}

setCurrentProgram :: Program -> Vm -> Vm
setCurrentProgram program vm =
  let programNbr = currentProgramNbr vm
      programs' = (programs vm) & element programNbr .~ program
  in vm { programs = programs' }

getCurrentProgram :: Vm -> Program
getCurrentProgram vm =
  (programs vm) !! (currentProgramNbr vm)
  
setCurrentProgramRegister :: RegisterNbr -> RegisterValue -> Vm -> Vm
setCurrentProgramRegister registerNbr value vm =
  let program = getCurrentProgram vm
      registerNbr' = fromIntegral registerNbr
      value' = fromIntegral value
      registers' = (registers program) & element registerNbr' .~ value'
      program' = program { registers = registers' }
  in setCurrentProgram program' vm

getCurrentProgramRegister :: RegisterNbr -> Vm -> RegisterValue
getCurrentProgramRegister registerNbr vm =
  let program = getCurrentProgram vm
  in fromIntegral $ (registers program) !! fromIntegral registerNbr

setCurrentProgramCarry :: Bool -> Vm -> Vm
setCurrentProgramCarry flag vm =
  let program = getCurrentProgram vm
      program' = program { carry = flag }
  in setCurrentProgram program' vm

getCurrentProgramPc :: Vm -> Offset
getCurrentProgramPc vm =
  let program = getCurrentProgram vm
  in pc program

setCurrentProgramPc :: Offset -> Vm -> Vm
setCurrentProgramPc offset vm =
  let program = getCurrentProgram vm
      program' = program { pc = offset }
  in setCurrentProgram program' vm

-- FIXME : implement
setMemoryByCurrentProgramPc :: Offset -> Word32 -> Int -> Vm -> Vm
setMemoryByCurrentProgramPc offset value size vm = vm

parameterValue :: Parameter -> Word32
parameterValue parameter = case parameter of
        PRegister value -> fromIntegral value
        PIndirect value -> fromIntegral value
        PDirect value -> fromIntegral value

-- FIXME : might not work, will see, otherwise use getValueFromMemory
-- rename this one to getParameterFromMemory
getValueFromMemory :: Read a => (Int -> Int) -> Parameter -> Int -> Vm -> a
getValueFromMemory f parameter size vm =
  let offset = parameterValue parameter
      pc = getCurrentProgramPc vm
      address = f $ fromIntegral (pc + offset)
      memory' = memory vm
      slice from to xs = B.take (to - from + 1) (B.drop from xs) 
      value = slice address (address + size) $ B.pack memory'
   in read $ show value

--getValueFromMemory :: Parameter -> (a -> Parameter) -> Vm -> a
--getValueFromMemory param size vm =
--  let value' = case param of _ offset -> getValueFromMemory offset size vm
--  in case value' of size value'' -> value''
