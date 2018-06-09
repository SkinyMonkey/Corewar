import Test.Hspec
import ParseAsmSpec
import CheckArgsSpec
import CodeGenerationSpec
import ComputeOffsetsSpec
import InstructionsSpec
import VmSpec
import UnpackInstructionSpec
import CheckInstructionSpec

main :: IO ()
main = hspec $ do
--  testCheckArgs
--  testParseAsm
--  testCodeGeneration
--  testComputeOffsets
--  testVm
--  testInstructions
--  testCheckInstructions
  testUnpackInstructions
