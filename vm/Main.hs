import System.Environment (getArgs)

import Debug.Trace

main :: IO ()
main = do
        args <- getArgs
        putStrLn "End VM"
