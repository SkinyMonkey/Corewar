import System.IO
import System.Environment (getArgs)

loadChampion fileName = do
  print fileName
  championFile <- openFile fileName ReadMode
  contents <- hGetContents championFile
  let list = words contents
  print list
  hClose championFile

main :: IO ()
main = do
        args <- getArgs
        mapM_ loadChampion args

--  map function args
--        putStrLn "Opening file : " ++  
