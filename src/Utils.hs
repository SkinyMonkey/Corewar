module Utils where

import Debug.Trace

slice from to str = drop from $ take to str

trace' x = trace ("$>" ++ show x) x

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
