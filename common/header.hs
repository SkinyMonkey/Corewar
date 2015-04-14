module Header () where

import Data.ByteString.Char8

-- Header for executable ".cor"
corewarExecMagic = "\x00\xea\x83\xf3"
magicLen = 4
progNameLength = 128
sizeContentLength = 8
commentLength = 2048

data Header = Header {
  magic :: Int,
  progName :: ByteString,
  progSize :: Int,
  comment :: ByteString
}	deriving (Show)
