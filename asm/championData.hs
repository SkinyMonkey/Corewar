module ChampionData (
  newChampionData,
  addLabel,
  addMetadata,
  addInstruction,
  incLineNbr,
  setCurrentLine,
  getHeader,
  getFileName,
  setFileName,
  getLineNbr,
  getCurrentLine,
  getInstructions,
  getByteCount,
  getLabelOffset,
  incCounter,
  resetByteCounter,
  ChampionData
) where

import Header
import Op
import Data.Word
import qualified Data.Map as Map  

-- FIXME : DEBUG
import Debug.Trace

getHeader :: ChampionData -> Header
getHeader self = header self

getFileName :: ChampionData -> String
getFileName self = fileName self

setFileName :: ChampionData -> String -> ChampionData
setFileName self fileName = self {fileName = fileName}

getLineNbr :: ChampionData -> Int
getLineNbr self = lineNbr self

getCurrentLine :: ChampionData -> String
getCurrentLine self = currentLine self

getInstructions self = instructions self

setCurrentLine :: ChampionData -> String -> ChampionData
setCurrentLine self line = self {currentLine = line}

getByteCount :: ChampionData -> Int
getByteCount self = byteCounter self

getLabelOffset :: ChampionData -> String -> Int
getLabelOffset self label = do
  let offset = Map.findWithDefault 0 label (labels self) in
    if offset == 0
    then error $ "Used label does not exist : " ++ label
    else offset

incLineNbr :: ChampionData -> ChampionData
incLineNbr self = self {lineNbr = nbr + 1}
  where nbr = getLineNbr self

-- FIXME : use common.hs definitions, indSize etc
argByteSize arg = 
  case parameterType of
    Register -> 4
    Direct -> 4
    Indirect -> 2
  where parameterType = fst arg 

argsByteSize :: Word8 -> [(ArgType, String)] -> Int
argsByteSize code args =
  if code `elem` noOpCodeInstructions
  then 2 -- FIXME : correct?
  else 2 + (foldl (+) 0 $ map argByteSize args)

-- INFO : instruction = (code, [(parameterType, argValue)])
addInstruction self op args =
  let code = getCode op
      byteSize = argsByteSize code args
      instruction = (code, args, byteSize)
      newInstructions = (instructions self)++[instruction]
      newByteCounter = (byteCounter self) + byteSize
  in
  self {instructions = newInstructions, byteCounter = newByteCounter }

addLabel _ label | trace ("addlabel: " ++ (init label)) False = undefined
addLabel self label = self {labels = Map.insert label offset labelsOffsets}
  where labelsOffsets = labels self
        offset = byteCounter self

addMetadata :: ChampionData -> String -> String -> ChampionData
addMetadata self "name" value = self {header = (setProgName (header self) value)}
addMetadata self "comment" value = self {header = (setComment (header self) value)}
addMetadata self _ _ = self

resetByteCounter :: ChampionData -> ChampionData
resetByteCounter self = self {byteCounter = 0}

incCounter :: ChampionData -> Word8 -> [(ArgType, String)] -> ChampionData
incCounter self code args =
  self {byteCounter = (byteCounter self) + (argsByteSize code args)}

data ChampionData = ChampionData {
  fileName :: String,
  currentLine :: String,
  lineNbr :: Int,
  byteCounter :: Int,
  instructions :: [(Word8,[(ArgType, String)], Int)],
  labels :: Map.Map String Int,
  header :: Header
} deriving (Show)

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
