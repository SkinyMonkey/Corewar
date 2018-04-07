module Header where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import Data.Binary.Put
import Data.Char
import GHC.Word

-- Header for executable ".cor"
corewarExecMagic :: String
corewarExecMagic = ['\x00','\xea','\x83','\xf3']

magicLen :: Int
magicLen = 4

progNameLength :: Int
progNameLength = 128

--sizeContentLength :: Int
--sizeContentLength = 8

commentLength ::Int
commentLength = 2048

rightPaddedString :: Int -> String -> String
rightPaddedString size string =
  string ++ [chr 0 | _ <- [1..size - length string ]]

data Header = Header {
  magic :: B.ByteString,
  progName :: String,
  progSize :: Word32,
  comment :: String
} deriving (Show, Eq)

setProgName :: Header -> String -> Header
setProgName self value = self {progName = value}

setComment :: Header -> String -> Header
setComment self value = self {comment = value}

setProgSize :: Header -> Word32 -> Header
setProgSize self value = self {progSize = value}

newHeader = Header
  (B.pack $ rightPaddedString magicLen corewarExecMagic)
  ""
  0
  ""
