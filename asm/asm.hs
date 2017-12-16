import System.Environment (getArgs)
import ParseAsm
import ComputeOffsets
--import CodeGeneration

import Debug.Trace

finished fileName step (False, _) = error $ step ++ " failed for " ++ fileName
finished fileName step (True, x) = (step ++ " complete for " ++ fileName, x)

generateChampion fileName = do
  let finishedStep = finished fileName
  content <- readFile fileName

  let parseRes = parseChampion fileName content
      (complete, championData) = finishedStep "Parsing" parseRes
  --traceIO $ show championData
  putStrLn complete

  -- FIXME : might not be OK : offset from beginning or
  -- indirect computed from current byte count?
  let championData' = computeLabelAdressing championData
      (complete, _) = finishedStep "Label offset computing" (True, "")
  --traceIO $ show championData'
  putStrLn complete

  return championData'
  -- FIXME : put data into a bytestring and write the file here!
  --         use cons
  --         use ByteString not ByteString.Lazy
  --         recheck page on bytstrings
--  binaryCode = generateCode championData offsets -- TODO : finished
--  writeFile corFileName
--  where corFileName = (take (length(fileName) - 2) fileName) ++ ".cor"

main :: IO ()
main = do
        args <- getArgs
        mapM_ generateChampion args
        putStrLn "End"
