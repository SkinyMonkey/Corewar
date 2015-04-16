module Header (
  generateHeader,
  setProgName,
  setComment,
  newHeader,
  Header
) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import Data.Binary.Put
import Data.Char
import GHC.Word

-- Header for executable ".cor"
corewarExecMagic :: [Char]
corewarExecMagic = ['\x00','\xea','\x83','\xf3']

magicLen :: Int
magicLen = 4

progNameLength :: Int
progNameLength = 128

--sizeContentLength :: Int
--sizeContentLength = 8

commentLength ::Int
commentLength = 2048

-- FIXME : modify following code to apply on Header record
rightPaddedString :: Int -> [Char] -> [Char]
rightPaddedString size string =
  string ++ [chr(0) | c <- [1..size - length(string)]]

serializeHeader :: [Char] -> [Char] -> GHC.Word.Word32 -> PutM ()
serializeHeader name comment progSize = do
  putByteString $ B.pack $ rightPaddedString magicLen corewarExecMagic
  putByteString $ B.pack $ rightPaddedString progNameLength name
  putWord32be progSize
  putByteString $ B.pack $ rightPaddedString commentLength comment

generateHeader :: [Char] -> [Char] -> GHC.Word.Word32 -> IO ()
generateHeader name comment progSize =B.writeFile "tests/res.cor"
  $ B.concat $ BL.toChunks $ runPut (serializeHeader name comment progSize)

data Header = Header {
  magic :: B.ByteString,
  progName :: String,
  progSize :: Int,
  comment :: String
}	deriving (Show)

setProgName :: Header -> String -> Header
setProgName self value = self {progName = value}

setComment :: Header -> String -> Header
setComment self value = self {comment = value}

newHeader = Header
  (B.pack $ rightPaddedString magicLen corewarExecMagic)
  ""
  0
  ""
