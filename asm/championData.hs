module ChampionData (
  newChampionData,
  addLabel,
  addMetadata,
  addInstruction,
  incLineNbr,
  setCurrentLine,
  getHeader,
  getFileName,
  getLineNbr,
  getCurrentLine,
  getInstructions,
  ChampionData
) where

import Header
import Op
import Data.Word
import qualified Data.Map as Map  

getHeader :: ChampionData -> Header
getHeader self = header self

getFileName :: ChampionData -> String
getFileName self = fileName self

getLineNbr :: ChampionData -> Int
getLineNbr self = lineNbr self

getCurrentLine :: ChampionData -> String
getCurrentLine self = currentLine self

getInstructions self = instructions self

setCurrentLine :: ChampionData -> String -> ChampionData
setCurrentLine self line = self {currentLine = line}

incLineNbr :: ChampionData -> ChampionData
incLineNbr self = self {lineNbr = nbr + 1}
  where nbr = getLineNbr self

-- FIXME : use common.hs definitions, indSize etc
argByteSize arg
  | parameterType == register = 4
  | parameterType == direct = 4
  | parameterType == indirect = 2
  | otherwise = error "Unknown parameter type : this should never happen." 
  where parameterType = fst arg 

argsByteSize args = map argByteSize args

byteCount :: [Char] -> [(Int, String)] -> Int
byteCount mnemonic args =
  if mnemonic `elem` noOpCodeMnemonics
  then 2 -- FIXME : correct?
  else 2 + (foldl (+) 0 $ argsByteSize args)
  where noOpCodeMnemonics = ["live", "zjmp", "fork", "lfork"]

addLabel self label = self {labels = Map.insert label offset labelsOffsets}
  where labelsOffsets = labels self
        offset = byteCounter self

-- instruction = (code, [(parameterType, argValue)])
addInstruction self mnemonic op args =
  self {instructions = (instructions self)++[instruction],
        byteCounter = (byteCounter self) + (byteCount mnemonic args)}
  where instruction = (getCode op, args)

addMetadata :: ChampionData -> String -> String -> ChampionData
addMetadata self "name" value = self {header = (setProgName (header self) value)}

addMetadata self "comment" value = self {header = (setComment (header self) value)}

addMetadata self _ _ = self

data ChampionData = ChampionData {
  fileName :: String,
  currentLine :: String,
  lineNbr :: Int,
  byteCounter :: Int,
  instructions :: [(Word8,[(Int, String)])],
  labels :: Map.Map String Int,
  header :: Header
}

newChampionData :: String -> ChampionData
newChampionData fileName = ChampionData fileName "" 0 0 [] Map.empty newHeader
