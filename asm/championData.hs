module ChampionData (
  newChampionData,
  addLabel,
  addMetadata,
  addInstruction,
  incLineNbr,
  setCurrentLine,
  getFileName,
  getLineNbr,
  getCurrentLine
) where

import Header

getFileName :: ChampionData -> String
getFileName self = fileName self

getLineNbr :: ChampionData -> Int
getLineNbr self = lineNbr self

getCurrentLine :: ChampionData -> String
getCurrentLine self = currentLine self

setCurrentLine :: ChampionData -> String -> ChampionData
setCurrentLine self line = self {currentLine = line}

incLineNbr :: ChampionData -> ChampionData
incLineNbr self = self {lineNbr = nbr + 1}
  where nbr = getLineNbr self

addLabel self label = self {labelFound = label:labels}
  where labels = labelFound self

-- FIXME : finish
addInstruction self op = self -- {instructions = 1:instructions}
--  where instructions = instructions self

addMetadata :: ChampionData -> String -> String -> ChampionData
addMetadata self "name" value = self {header = (setProgName (header self) value)}

--addMetadata self "comment" value = self {header = header}
--  where header = (setComment (header self) value)

addMetadata self _ _ = self

data ChampionData = ChampionData {
  fileName :: String,
  lineNbr :: Int,
  currentLine :: String,
  labelFound :: [String],
  header :: Header,
  instructions :: [Int]
--  labelCalled :: [],
}

--data Instruction = Instruction {
--  code :: Word8,
--  args :: [(Int, Int)]
--}

newChampionData :: String -> ChampionData
newChampionData fileName = ChampionData fileName 0 "" [] newHeader []
