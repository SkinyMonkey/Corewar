import Test.Hspec
--import CheckArgsSpec
--import ParseAsmSpec
--import CodeGenerationSpec
--import ComputeOffsetsSpec
--import InstructionsSpec
--import VmSpec
import UnpackInstructionSpec

main :: IO ()
main = hspec $ do
--  testCheckArgs
--  testParseAsm
--  testCodeGeneration
--  testComputeOffsets
--  testInstructions
  testUnpackInstructions
--  testVm
