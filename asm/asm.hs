import System.Environment (getArgs)
import ParseAsm
import CodeGeneration

generateChampion fileName = do
  res <- parseChampion fileName
  generateCode res

main :: IO ()
main = do
        args <- getArgs
        mapM_ generateChampion args
        putStrLn "End"
