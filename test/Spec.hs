import Test.Hspec
import CheckArgsSpec
import ParseAsmSpec

main :: IO ()
main = hspec $ do
  testCheckArgs
  testParseAsm
