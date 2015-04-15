module ChampionData (
  newChampionData,
  incLineNbr,
  setCurrentLine,
  getLineNbr,
  getCurrentLine
) where

getLineNbr self = lineNbr self
getCurrentLine self = currentLine self

setCurrentLine self line = self {currentLine = line}
incLineNbr self = self {lineNbr = nbr + 1}
  where nbr = getLineNbr self

data ChampionData = ChampionData {
  lineNbr :: Int,
  currentLine :: String
--  labelFound :: [],
--  labelCalled :: [],
--  instructions :: [Instruction]
}

--data Instruction = Instruction {
--  code :: Word8,
--  args :: [(Int, Int)]
--}

newChampionData line = ChampionData 0 line-- []
