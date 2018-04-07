import Test.Hspec
--import CheckArgsSpec
--import ParseAsmSpec
--import CodeGenerationSpec
import ComputeOffsetsSpec

main :: IO ()
main = hspec $ do
--  testCheckArgs
--  testParseAsm
--  testCodeGeneration

  testComputeOffsets
