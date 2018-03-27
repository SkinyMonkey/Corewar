import System.Environment (getArgs)
import ParseAsm
import ComputeOffsets
--import CodeGeneration

import ChampionData

import Debug.Trace

finished filename step Nothing = error $ step ++ " failed for " ++ filename
finished filename step (Just x)  = (step ++ " complete for " ++ filename, x)

-- TODO : this should be the only finished
finishedParsing filename step (Left errors) = error $ step ++ " failed for " ++ filename ++ errors
finishedParsing filename step (Right x) = (step ++ " complete for " ++ filename, x)

generateChampion filename = do
  let finishedStep = finished filename
  content <- readFile filename

  let parseRes = parseChampion filename content
      (complete, championData) = finishedParsing filename "Parsing" parseRes
--  traceIO $ show championData
  putStrLn complete

  -- FIXME : might not be OK : offset from beginning or
  -- indirect computed from current byte count?
  let championData' = computeLabelAdressing championData
      (complete, _) = finishedStep "Label offset computing" $ Just ""
  traceIO $ show championData'
  putStrLn complete

  return championData'
  -- FIXME : put data into a bytestring and write the file here!
  --         use cons
  --         use ByteString not ByteString.Lazy
  --         recheck page on bytstrings
--  binaryCode = generateCode championData offsets -- TODO : finished
--  writeFile corFileName
--  where corFileName = (take (length(filename) - 2) filename) ++ ".cor"

main :: IO ()
main = do
        args <- getArgs
        mapM_ generateChampion args
        putStrLn "End"
