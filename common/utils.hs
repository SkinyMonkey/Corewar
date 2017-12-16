module Utils (wordsWhen, trace') where

import Debug.Trace

trace' x = trace ("$>" ++ show x) x

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
