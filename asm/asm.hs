import System.Environment (getArgs)
import ParseAsm
--import CodeGeneration

import Debug.Trace

-- TODO : move finished here
-- finished (True, cd) | trace (show (getInstructions cd)) False = undefined
-- finished (True, cd) = do
--   putStrLn $ "Compilation complete for " ++ getFileName cd
--   return cd
-- finished (False, cd) = error $ "Compilation failed for " ++ getFileName cd

generateChampion fileName = do
  content <- readFile fileName
  let championData = parseChampion fileName content -- TODO : finished
  error $ show $ snd championData
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
