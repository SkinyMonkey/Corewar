import ParseAsm
import ChampionData

championData = newChampionData "test"

parseMetadataOk = 
  let instruction = words ".name \"test\""
      (result, championData') = parseMetadata instruction championData
  in (result, getProgName championData')

--testParseMetatada =

main :: IO ()
main = do
  let result = parseMetadataOk
  putStrLn $ show result
