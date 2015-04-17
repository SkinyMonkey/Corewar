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

addLabel self label = self {labelFound = labels++[label]}
  where labels = labelFound self

-- instruction = (code, [(argType, argValue)])
addInstruction self op args = self {instructions = (instructions self)++[instruction]}
  where instruction = (getCode op, args)

addMetadata :: ChampionData -> String -> String -> ChampionData
addMetadata self "name" value = self {header = (setProgName (header self) value)}

addMetadata self "comment" value = self {header = (setComment (header self) value)}

addMetadata self field value = self

data ChampionData = ChampionData {
  fileName :: String,
  lineNbr :: Int,
  currentLine :: String,
  labelFound :: [String],
  header :: Header,
  instructions :: [(Word8,[(Int, String)])]
}

newChampionData :: String -> ChampionData
newChampionData fileName = ChampionData fileName 0 "" [] newHeader []
