module Vm.Vm where

import Data.Int
import Data.Word
import Data.Maybe
import Data.Bits
import Foreign.Storable

import Control.Lens
import Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

import Utils
import Op

data Program = Program {
  number :: Int,
  name :: String,
  registers :: [Word32],
  pc :: Offset, -- eip
  carry :: Bool,
  alive :: Bool,
  cyclesLeft :: Int -- before next instruction
} deriving (Show, Eq)

newProgram :: Int -> String -> Offset -> Program
newProgram number name pcOffset = Program {
  number = number,
  name = name,
  registers = replicate regNumber 0,
  pc = pcOffset,
  carry = False,
  alive = True,
  cyclesLeft = 0
}

data Vm = Vm {
  programs :: [Program],
  currentProgramNbr :: Int,
  memory :: B.ByteString,
  graphicMemory :: B.ByteString,
  affBuffer :: String
} deriving (Show, Eq)

newVm :: Vm
newVm = Vm {
  programs = [],
  currentProgramNbr = 0,
  memory = B.pack $ replicate memSize 0,
  graphicMemory = B.pack $ replicate memSize 0,
  affBuffer = ""
}

w8stoW32 :: [Word8] -> Word32
w8stoW32 = foldl conversion 0
  where conversion a o = ( a `shiftL` 8) .|. fromIntegral o

w32tow8s :: Word32 -> [Word8]
w32tow8s w = [
    fromIntegral ( w `shiftR` 24),
    fromIntegral ( w `shiftR` 16),
    fromIntegral ( w `shiftR` 8),
    fromIntegral w
  ]

modMemSize = flip mod memSize

setMemory :: Offset -> B.ByteString -> B.ByteString -> B.ByteString
setMemory offset memory content =
  let contentLength = B.length content
      offset' = modMemSize $ fromIntegral offset
      memoryBeforeContent = bslice 0 (fromIntegral offset) memory
      memoryAfterContent = bslice (fromIntegral offset + contentLength) (B.length memory) memory
  in B.concat [ memoryBeforeContent, content, memoryAfterContent ]

setGraphicalMemory :: Offset -> Int -> B.ByteString -> B.ByteString -> B.ByteString
setGraphicalMemory offset championNbr memory content = 
  let graphicInstructions = B.pack $ replicate (B.length content) (fromIntegral championNbr)
  in setMemory offset memory graphicInstructions

setMemorys :: Offset -> Int -> B.ByteString -> Vm -> Vm
setMemorys offset championNbr content vm =
  let memory' = setMemory offset (memory vm) content
      graphicMemory' = setGraphicalMemory offset championNbr (graphicMemory vm) content
  in vm { memory = memory', graphicMemory = graphicMemory' }

word32ToBytestring :: Word32 -> B.ByteString
word32ToBytestring = B.pack . w32tow8s

byteStringToWord32 :: B.ByteString -> Word32
byteStringToWord32 = w8stoW32 . B.unpack

insertProgram :: Int -> Int -> B.ByteString -> Vm -> Vm
insertProgram championsNbr championNbr programContent vm =
  let name = filter (/='\NUL') $ B8.unpack $ bslice nameOffset nameSize programContent
      progSize = byteStringToWord32 $ bslice progSizeOffset progSizeSize programContent
      instructions = bslice (headerSize + 8) (B.length programContent) programContent
      offset = fromIntegral $ championNbr * memSize `div` championsNbr
      cyclesLeft = B.head instructions

      program = newProgram championNbr name offset
      vm' = vm { programs = programs vm ++ [program] }
  in setMemorys offset (championNbr + 1) instructions vm'

setCurrentProgram :: Program -> Vm -> Vm
setCurrentProgram program vm =
  let championNbr = currentProgramNbr vm
      programs' = programs vm & ix championNbr .~ program
  in vm { programs = programs' }

getProgramAt :: Int -> Vm -> Program
getProgramAt index vm = (programs vm) !! index

setProgramAt :: Program -> Int -> Vm -> Vm
setProgramAt program index vm =
  let programs' = programs vm & ix index .~ program
  in vm { programs = programs' }

getCurrentProgram :: Vm -> Program
getCurrentProgram vm = programs vm !! currentProgramNbr vm

setCurrentProgramNbr :: Program -> Vm -> Vm
setCurrentProgramNbr program vm =
  vm { currentProgramNbr = number program }
  
setCurrentProgramRegister :: RegisterNbr -> RegisterValue -> Vm -> Vm
setCurrentProgramRegister registerNbr value vm =
  let program = getCurrentProgram vm
      registerNbr' = fromIntegral registerNbr
      registers' = registers program & ix registerNbr' .~ fromIntegral value
      program' = program { registers = registers' }
  in setCurrentProgram program' vm

getCurrentProgramRegister :: RegisterNbr -> Vm -> RegisterValue
getCurrentProgramRegister registerNbr vm =
  let program = getCurrentProgram vm
  in fromIntegral $ registers program !! fromIntegral registerNbr

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

getCurrentChampionNumber :: Vm -> Int
getCurrentChampionNumber vm = number $ getCurrentProgram vm

setCurrentProgramPc :: Offset -> Vm -> Vm
setCurrentProgramPc offset vm =
  let program = getCurrentProgram vm
      program' = program { pc = fromIntegral $ modMemSize $ fromIntegral offset }
  in setCurrentProgram program' vm

updateMemory :: Offset -> Word32 -> Int -> Vm -> Vm
updateMemory offset value size vm =
  let toInsert = B.drop (4 - size) $ word32ToBytestring value
      championNbr = getCurrentChampionNumber vm
  in setMemorys offset championNbr toInsert vm

setMemoryByCurrentProgramPc :: (Int -> Int) -> Offset -> Word32 -> Int -> Vm -> Vm
setMemoryByCurrentProgramPc f offset value size vm =
  let pc = fromIntegral $ getCurrentProgramPc vm
      offset' = fromIntegral $ pc + f (fromIntegral offset)
  in updateMemory offset' value size vm

getValueFromMemory :: (Int -> Int) -> Offset -> Int -> Vm -> Word32
getValueFromMemory f offset size vm =
  let pc = fromIntegral $ getCurrentProgramPc vm
      offset' = modMemSize $ pc + (f $ fromIntegral offset)
      value = bslice offset' (offset' + size) (memory vm)
   in w8stoW32 $ B.unpack value

incrementCurrentProgramPc :: Int -> Vm -> Vm
incrementCurrentProgramPc size vm =
  let program = getCurrentProgram vm
  in setCurrentProgram (program { pc = pc program + fromIntegral size }) vm

decrementCurrentProgramCycleLeft :: Vm -> Vm
decrementCurrentProgramCycleLeft vm =
  let program = getCurrentProgram vm
      program' = program { cyclesLeft = cyclesLeft program - 1 }
  in setCurrentProgram program' vm

setCurrentProgramCycleLeft :: Int -> Vm -> Vm
setCurrentProgramCycleLeft cycles vm =
  let program = getCurrentProgram vm
      program' = program { cyclesLeft = cycles }
  in setCurrentProgram program' vm

getParameterValue :: (Int -> Int) -> Parameter -> Int -> Vm -> Word32
getParameterValue f param size vm = case param of
  Register registerNbr -> fromIntegral $ getCurrentProgramRegister registerNbr vm
  Direct value -> fromIntegral value
  Indirect value -> getValueFromMemory f (fromIntegral value) size vm
