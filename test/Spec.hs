import Test.Hspec
import CheckArgsSpec
import ParseAsmSpec
import CodeGenerationSpec
import ComputeOffsetsSpec
import InstructionsSpec
-- import VmSpec

main :: IO ()
main = hspec $ do
--  testCheckArgs
--  testParseAsm
--  testCodeGeneration
--  testComputeOffsets
  testInstructions
-- testVm
