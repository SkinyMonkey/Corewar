module Asm.ChampionData where

-- FIXME : DEBUG
import Debug.Trace
import Data.Word
import qualified Data.Map as Map
import Data.Maybe
import Foreign.Storable

import Asm.Header
import Op

type Instruction = (Word8, [Parameter], Offset)
type EvaluatedInstruction = (Word8, [Parameter])

getHeader :: ChampionData -> Header
getHeader = header

getFileName :: ChampionData -> String
getFileName = fileName

setFileName :: ChampionData -> String -> ChampionData
setFileName self fileName = self {fileName = fileName}

getLineNbr :: ChampionData -> Int
getLineNbr = lineNbr

getCurrentLine :: ChampionData -> String
getCurrentLine = currentLine

getInstructions = instructions

setInstructions self instructions = self {instructions = instructions}

setCurrentLine :: ChampionData -> String -> ChampionData
setCurrentLine self line = self {currentLine = line}

getByteCount :: ChampionData -> Word32
getByteCount = byteCounter

getLabelOffset :: ChampionData -> String -> Offset
getLabelOffset self label =
  let offset = Map.lookup label (labels self)
  in fromMaybe (error $ "Used label does not exist : " ++ label) offset

incLineNbr :: ChampionData -> ChampionData
incLineNbr self = self {lineNbr = nbr + 1}
  where nbr = getLineNbr self

-- FIXME : use sizeOf Word32, Word16
argByteSize :: Bool -> Parameter -> Word32
argByteSize instructionHasIndex arg =
  case arg of
    Register _ -> 1
    Direct _ -> if instructionHasIndex then 2 else 4
    Indirect _ -> 2
    Label _ -> 2

-- FIXME : create an OpCode type
--         use sizeOf code
argsByteSize :: Word8 -> [Parameter] -> Word32
argsByteSize code args =
  let argSize = sum (map (argByteSize (code `elem` haveIndexInstructions)) args)
  in if code `elem` noOpCodeInstructions
     then 1 + argSize -- one byte for the instruction code
     else 2 + argSize -- one for instruction code, one for the opcode

addInstruction self op args =
  let code = getCode op
      offset = getByteCount self
      instruction = (code, args, offset)
      newInstructions = instructions self++[instruction]
  in
  incCounter (self {instructions = newInstructions}) code args

addLabel self label = self {labels = Map.insert label offset labelsOffsets}
  where labelsOffsets = labels self
        offset = byteCounter self

addMetadata :: ChampionData -> String -> String -> ChampionData
addMetadata self "name" value = self {header = setProgName (header self) value}
addMetadata self "comment" value = self {header = setComment (header self) value}
addMetadata self _ _ = self

getProgName :: ChampionData -> String
getProgName self = progName header
  where header = getHeader self

resetByteCounter :: ChampionData -> ChampionData
resetByteCounter self = self {byteCounter = 0}

-- For testing purpose
setByteCounter :: ChampionData -> Word32 -> ChampionData
setByteCounter self value = self {byteCounter = value}

incCounter :: ChampionData -> Word8 -> [Parameter] -> ChampionData
incCounter self code args =
  let newByteCounter = byteCounter self + argsByteSize code args
      newHeader = setProgSize (header self) newByteCounter
  in self {byteCounter = newByteCounter, header = newHeader }

data ChampionData = ChampionData {
  fileName :: String,
  currentLine :: String,
  lineNbr :: Int,
  byteCounter :: Word32,
  instructions :: [Instruction],
  labels :: Map.Map String Offset,
  header :: Header
} deriving (Show, Eq)

newChampionData :: String -> ChampionData
newChampionData fileName = ChampionData {
  fileName = fileName,
  currentLine = "",
  lineNbr = 0,
  byteCounter = 0,
  instructions = [],
  labels = Map.empty,
  header = newHeader
}
