import System.Environment (getArgs)
import ParseAsm
import CodeGeneration

generateChampion fileName = do
  parseRes <- parseChampion fileName
  generateRes <- generateCode parseRes
  return generateRes

main :: IO ()
main = do
        args <- getArgs
        mapM_ generateChampion args
        putStrLn "End"
