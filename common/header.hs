module Header (
  generateHeader
) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import Data.Binary.Put
import Data.Char

-- Header for executable ".cor"
corewarExecMagic = ['\x00','\xea','\x83','\xf3']
magicLen = 4
progNameLength = 128
sizeContentLength = 8
commentLength = 2048

rightPaddedString size string =
  string ++ [chr(0) | c <- [1..size - length(string)]]

serializeHeader name comment progSize = do
  putByteString $ B.pack $ rightPaddedString magicLen corewarExecMagic
  putByteString $ B.pack $ rightPaddedString progNameLength name
  putWord32be progSize
  putByteString $ B.pack $ rightPaddedString commentLength comment

generateHeader name comment progSize =B.writeFile "tests/res.cor"
  $ B.concat $ BL.toChunks $ runPut (serializeHeader name comment progSize)

--data Header = Header {
--  magic :: Int,
--  progName :: ByteString,
--  progSize :: Int,
--  comment :: ByteString
--}	deriving (Show)
