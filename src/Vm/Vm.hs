module Vm.Vm where

import Data.Int
import Data.Word
import Data.Maybe

-- FIXME : can be changed for a [ until index ] ++ element ++ [ after index ]
import Control.Lens
import Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

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
  memory :: B.ByteString,
  affBuffer :: String
} deriving (Show, Eq)

newVm :: [Program] -> Vm
newVm programs = Vm {
  programs = programs,
  currentProgramNbr = 0,
  memory = B.pack $ replicate memSize 0,
  affBuffer = ""
}

setCurrentProgram :: Program -> Vm -> Vm
setCurrentProgram program vm =
  let programNbr = currentProgramNbr vm
      programs' = (programs vm) & ix programNbr .~ program
  in vm { programs = programs' }

getCurrentProgram :: Vm -> Program
getCurrentProgram vm =
  (programs vm) !! (currentProgramNbr vm)
  
setCurrentProgramRegister :: RegisterNbr -> RegisterValue -> Vm -> Vm
setCurrentProgramRegister registerNbr value vm =
  let program = getCurrentProgram vm
      registerNbr' = fromIntegral registerNbr
      value' = fromIntegral value
      registers' = (registers program) & ix registerNbr' .~ value'
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

getCurrentProgramCarry :: Vm -> Bool
getCurrentProgramCarry vm = carry $ getCurrentProgram vm

getCurrentProgramPc :: Vm -> Offset
getCurrentProgramPc vm =
  let program = getCurrentProgram vm
  in pc program

setCurrentProgramPc :: Offset -> Vm -> Vm
setCurrentProgramPc offset vm =
  let program = getCurrentProgram vm
      program' = program { pc = offset }
  in setCurrentProgram program' vm

updateMemory' :: BL.ByteString -> Offset -> BL.ByteString -> Int64 -> BL.ByteString
updateMemory' memory offset value size =
  let value' = value ^? ix size
  in if size >= 0 && isJust value'
  then let offset' = (fromIntegral offset) + size
           memory' = memory & ix offset' .~ fromJust value'
       in updateMemory' memory' offset value (size - 1)
  else memory

updateMemory :: B.ByteString -> Offset -> Word32 -> Int -> B.ByteString
updateMemory memory offset value size =
  let unpackedValue = (toLazyByteString . word32BE) $ fromIntegral value
      lmemory = BL.fromStrict memory
      size' = fromIntegral size :: Int64
  in B.concat . BL.toChunks $ updateMemory' lmemory offset unpackedValue (size' - 1)

modMemSize = flip mod memSize

setMemoryByCurrentProgramPc :: (Int -> Int) -> Offset -> Word32 -> Int -> Vm -> Vm
setMemoryByCurrentProgramPc f offset value size vm =
  let pc = fromIntegral $ getCurrentProgramPc vm
      offset' = fromIntegral $ modMemSize $ pc + (f $ fromIntegral offset)
      packedMemory = memory vm
      memory' = updateMemory packedMemory offset' value size
   in vm { memory = memory' }

parameterValue :: Parameter -> Word32
parameterValue parameter = case parameter of
        PRegister value -> fromIntegral value
        PIndirect value -> fromIntegral value
        PDirect value -> fromIntegral value

-- rename this one to getParameterFromMemory
getValueFromMemory :: Read a => (Int -> Int) -> Parameter -> Int -> Vm -> a
getValueFromMemory f parameter size vm =
  let pc = fromIntegral $ getCurrentProgramPc vm
      offset = f $ fromIntegral $ parameterValue parameter
      offset' = modMemSize $ pc + offset
      memory' = memory vm
      slice from to xs = B.take (to - from + 1) (B.drop from xs) 
      value = slice offset' (offset' + size) memory'
   in read $ show value
