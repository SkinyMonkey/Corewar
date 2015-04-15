import System.Environment (getArgs)
import ParseAsm

main :: IO ()
main = do
        args <- getArgs
        mapM_ generateChampion args
