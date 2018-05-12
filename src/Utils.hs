module Utils where

import qualified Data.ByteString as B

import Debug.Trace

bslice from to xs = B.drop from $ B.take to xs

slice from to xs = drop from $ take to xs

updateAt :: Int -> [a] -> a -> [a]
updateAt index xs value =
  let beforeIndex = slice 0 index xs
      afterIndex = slice (index + 1) (length xs) xs
  in concat [beforeIndex, [value], afterIndex]

trace' x = trace ("$>" ++ show x) x

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

lpad m xs = let ys = take m xs in replicate (m - length ys) '0' ++ ys
