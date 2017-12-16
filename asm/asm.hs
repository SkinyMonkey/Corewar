import System.Environment (getArgs)
import ParseAsm
import ComputeOffsets
--import CodeGeneration

import Debug.Trace

finished step fileName (True, x) = (step ++ " complete for " ++ fileName, x)
finished step fileName (False, _) = error $ step ++ " failed for " ++ fileName

--generateChampion :: String -> IO (ParseResult)
generateChampion fileName = do
  content <- readFile fileName
  let (_, championData) = parseChampion fileName content -- TODO : finished
  traceIO $ show $ championData
  -- FIXME : might not be OK : offset from beginning or
  -- indirect computed from current byte count?
  let championData' = computeLabelAdressing championData
  traceIO $ show $ championData'
  return championData
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
