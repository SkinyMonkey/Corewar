module VmSpec where

import Data.Word
import Test.Hspec
import Test.QuickCheck
import qualified Data.ByteString.Char8 as B8

import Vm.Vm
import Utils
import Op

testVm =
  describe "Vm" $ do
   -- NOTE: because the program is alone
   -- it is placed at 2300 something
   -- so we force the pc to 0
   let programContent = B8.pack $ replicate 20 '\0'
       vm' = setCurrentProgramPc 0 $ insertProgram 1 1 programContent newVm
       program = head (programs vm')
       vm = setCurrentProgramNbr program vm'

   it "w8stoW32 && w32tow8s" $ do
     let w = 10000000
     (w8stoW32 $ w32tow8s w) `shouldBe` w

   it "getCurrentProgram" $ do
     getCurrentProgram vm `shouldBe` program

   it "setCurrentProgram" $ do
     let program' = program { carry = True }
         vm' = setCurrentProgram program' vm
     getCurrentProgram vm' `shouldBe` program'

   it "setCurrentProgramRegister" $ do
     let vm' = setCurrentProgramRegister 0 32 vm
         regs = registers $ getCurrentProgram vm'
     head regs `shouldBe` 32

   it "getCurrentProgramRegister" $ do
     let reg = getCurrentProgramRegister 1 vm
     reg `shouldBe` 0

   it "getCurrentProgramCarry" $ do
     let carry = getCurrentProgramCarry vm
     carry `shouldBe` False

   it "setCurrentProgramCarry" $ do
     let vm' = setCurrentProgramCarry True vm
         carry = getCurrentProgramCarry vm'
     carry `shouldBe` True

   it "getCurrentProgramPc" $ do
     let pc = getCurrentProgramPc vm
     pc `shouldBe` 0

   it "setCurrentProgramPc" $ do
     let vm' = setCurrentProgramPc 32 vm
         pc = getCurrentProgramPc vm'
     pc `shouldBe` 32

   describe "Memory"$ do
    it "setMemory" $ do
      let offset = 0
          content = B8.pack "666"
          memory = B8.pack "0123456789"
          result = B8.pack "6663456789"
      setMemory offset memory content `shouldBe` result

      let offset = 4
          result' = B8.pack "0123666789"
      setMemory offset memory content `shouldBe` result'

    it "setGraphicalMemory" $ do
      let offset = 0
          championNbr = 1
          content = B8.pack "666"
          graphicMemory = B8.pack "\0\0\0\0\0\0"
          graphicResult = B8.pack "\1\1\1\0\0\0"

      setGraphicalMemory offset championNbr graphicMemory content `shouldBe` graphicResult

    it "setMemorys" $ do
      let offset = 0
          championNbr = 1
          content = B8.pack "666"
          result = B8.pack "666\0\0\0"
          graphicResult = B8.pack "\1\1\1\0\0\0"
          vm' = setMemorys offset championNbr content vm

      bslice 0 6 (memory vm') `shouldBe` result
      bslice 0 6 (graphicMemory vm') `shouldBe` graphicResult

    it "updateMemory" $ do
      let offset = 0
          value = 255
          size = 1
          result = B8.pack "\255\NUL\NUL\NUL\NUL\NUL"
          graphicResult = B8.pack "\1\0\0\0\0\0"
          vm' = updateMemory offset value size vm

      bslice 0 6 (memory vm') `shouldBe` result
      bslice 0 6 (graphicMemory vm') `shouldBe` graphicResult

    it "setMemoryByCurrentProgramPc" $ do
      let f = id
          offset = 2
          pc = 2
          value = 255
          size = 1
          vm' = setMemoryByCurrentProgramPc f offset value size $ (setCurrentProgramPc pc vm)
          result = B8.pack "\NUL\NUL\NUL\NUL\255\NUL"
          graphicResult = B8.pack "\0\0\0\0\1\0"

      bslice 0 6 (memory vm') `shouldBe` result
      bslice 0 6 (graphicMemory vm') `shouldBe` graphicResult
      B8.length (memory vm') `shouldBe` memSize
      B8.length (graphicMemory vm') `shouldBe` memSize

    it "getValueFromMemory" $ do
      let f = id
          value = 255
          valueSize = 1

          pc = 4 -- 0000100000 where 1 is the position
          setMemoryOffset = 4 -- 0000000010 where 1 is the position of the write
          vm' = setMemoryByCurrentProgramPc f setMemoryOffset value valueSize $ (setCurrentProgramPc pc vm)
   
          -- read 4 bytes at pc + readMemoryOffset
          readMemoryOffset = 1 -- 0000010000 where 1 is the position of the read
          size = 4
          result = getValueFromMemory f readMemoryOffset size vm'

      result `shouldBe` 255

    describe "getParameterValue" $ do
      it "get the value of a register from the vm" $ do
        let f = id
            param = Register 1
            size = 1 -- byte
            value = 12
            vm' = setCurrentProgramRegister 1 (fromIntegral value :: Word8) vm

        getParameterValue f param size vm' `shouldBe` value

      it "get the value of a direct" $ do
        let f = id
            param = Direct 1
            size = 1 -- byte

        getParameterValue f param size vm `shouldBe` 1

      describe "get the value of an indirect" $ do
        let f = id
            value = 255

        it "reads an int at offset 0" $ do
          let size = 4 -- bytes
              setMemoryOffset = 0
              param = Indirect (fromIntegral setMemoryOffset)
              vm' = setMemoryByCurrentProgramPc f (fromIntegral setMemoryOffset) value size vm

          getParameterValue f param size vm' `shouldBe` value

        it "reads a short at offset 1" $ do
          let size = 2 -- byte
              setMemoryOffset = 1
              param = Indirect (fromIntegral setMemoryOffset)
              vm' = setMemoryByCurrentProgramPc f (fromIntegral setMemoryOffset) value size vm

          getParameterValue f param size vm' `shouldBe` value

        it "reads a byte at offset 3" $ do
          let size = 1 -- byte
              setMemoryOffset = 3
              param = Indirect (fromIntegral setMemoryOffset)
              vm' = setMemoryByCurrentProgramPc f (fromIntegral setMemoryOffset) value size vm

          getParameterValue f param size vm' `shouldBe` value
