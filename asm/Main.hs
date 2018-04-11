import System.Environment (getArgs)
import Asm.Parsing.ParseAsm
import Asm.Generation.ComputeOffsets
import Asm.Generation.CodeGeneration

import Asm.ChampionData
import Asm.Header

import Debug.Trace

finished :: String -> String -> Maybe a -> (String, a)
finished filename step Nothing = error $ step ++ " failed for " ++ filename
finished filename step (Just x)  = (step ++ " complete for " ++ filename, x)

-- TODO : this should be the only finished
finishedParsing :: String -> String -> Either String a -> (String, a)
finishedParsing filename step (Left errors) = error $ step ++ " failed for " ++ filename ++ errors
finishedParsing filename step (Right x) = (step ++ " complete for " ++ filename, x)

generateChampion :: FilePath -> IO ()
generateChampion filename = do
  let finishedStep = finished filename
  content <- readFile filename

  let parseRes = parseChampion filename content
      (completeParsing, championData) = finishedParsing filename "Parsing" parseRes
--  traceIO $ show championData
  putStrLn completeParsing

  let evaluatedInstuctions = computeLabelAdressing championData
      (completeLabelComputing, _) = finishedStep "Label offset computing" $ Just ""
--  traceIO $ show evaluatedInstuctions
  putStrLn completeLabelComputing

  -- FIXME : 
  --         use cons
  --         use ByteString not ByteString.Lazy
  --         recheck page on bytstrings

  let getCorFileName f = take (length f - 2) f ++ ".cor"
      corFileName = getCorFileName $ getFileName championData
  writeChampion corFileName championData evaluatedInstuctions
  putStrLn $ "Writing done in : " ++ corFileName

main :: IO ()
main = do
        args <- getArgs
        mapM_ generateChampion args
        putStrLn "End Asm"
