module Utils where

import qualified Data.ByteString as B

import Debug.Trace

bslice from to str = B.drop from $ B.take to str

slice from to str = drop from $ take to str

trace' x = trace ("$>" ++ show x) x

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

lpad m xs = let ys = take m xs in replicate (m - length ys) '0' ++ ys
