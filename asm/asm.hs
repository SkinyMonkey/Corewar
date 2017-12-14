import System.Environment (getArgs)
import ParseBase
import ParseAsm
import ChampionData
--import CodeGeneration

import Debug.Trace

finished step fileName (True, x) = (step ++ " complete for " ++ fileName, x)
finished step fileName (False, x) = error $ step ++ " failed for " ++ fileName

--generateChampion :: String -> IO (ParseResult)
generateChampion fileName = do
  content <- readFile fileName
  let (_, championData) = parseChampion fileName content -- TODO : finished
  traceIO $ show $ championData
  return championData
--  offsets = computeOffsets championData
--  binaryCode = generateCode championData offsets -- TODO : finished
--  writeFile corFileName
--  where corFileName = (take (length(fileName) - 2) fileName) ++ ".cor"

main :: IO ()
main = do
        args <- getArgs
        mapM_ generateChampion args
        putStrLn "End"
