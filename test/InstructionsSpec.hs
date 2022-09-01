module InstructionsSpec where

import Test.Hspec
import Test.QuickCheck
import qualified Data.ByteString.Char8 as B8
import Data.Word

import Utils
import Op
import Vm.Vm
import Vm.Instructions

programContent = B8.pack $ replicate 20 '\0'

insertTestProgram championsNbr vm championNbr =
  insertProgram championsNbr championNbr programContent vm

shouldFail f params vm =
  let result = f params vm
  in result `shouldBe` Nothing

testInstructions =
  describe "Instructions" $ do
   let baseVm = newVm
       championNbrs = [1, 2]
       championsNbr = length championNbrs
       vm' = foldl (insertTestProgram championsNbr) baseVm championNbrs
       program = head (programs vm)
       vm = setCurrentProgramNbr program vm'

   describe "live" $ do 
    it "set a program as alive" $ do
      let program' = program { alive = False }
          params = [Direct 1]
          result = live params $ setCurrentProgram program' vm

      (alive $ getCurrentProgram $ result) `shouldBe` True

   describe "ld_" $ do
    let registerNbr = 1
        value = 42 :: Word8

    -- FIXME : pass offset to test modIdxMod?
        ldTest f = do
          let ldTest_ vm params registerNbr value carry = do
                let result = ld_ f params vm

                getCurrentProgramRegister registerNbr result `shouldBe` value
                getCurrentProgramCarry result `shouldBe` carry

          it "loads a value from memory into a register based on a direct" $ do
            let carry = True
                params = [Direct $ fromIntegral value, Register registerNbr]

            ldTest_ vm params registerNbr value carry

          it "loads a value from memory into a register based on an indirect" $ do
            let offset = 4
                carry = True
                vm' = setMemoryByCurrentProgramPc id offset (fromIntegral value) 1 vm
                params = [Indirect $ fromIntegral offset, Register registerNbr]

            ldTest_ vm' params registerNbr value carry

          it "loads a value from memory into a register, value == 0 -> carry == false" $ do
            let carry = False
                params = [Direct 0, Register registerNbr]

            ldTest_ vm params registerNbr 0 carry

    describe "ld" $ do
    -- FIXME : test modulo modIdxMod is effective
      ldTest modIdxMod

    describe "lld" $ do
      ldTest id

   describe "st" $ do
      let value = 42
          srcRegisterNbr = 1
          vm' = setCurrentProgramRegister srcRegisterNbr value vm

      it "stores the value of a register in a register" $ do
        let dstRegisterNbr = 2
            params = [Register srcRegisterNbr, Register dstRegisterNbr]
            result = st params vm'

        getCurrentProgramRegister dstRegisterNbr result `shouldBe` value

      it "stores the value of a register in memory" $ do
        let size = 1
            offset = 4
            params = [Register srcRegisterNbr, Indirect $ fromIntegral offset]
            result = st params vm'
 
        getValueFromMemory id offset size result `shouldBe` fromIntegral value
        -- FIXME : test modulo modIdxMod is effective

   describe "registerOperation" $ do
      let r1 = 1
          r2 = 2
          r3 = 3
          value = 42

      it "add two registers and st the result in a third" $ do
        let vm' = setCurrentProgramRegister r1 (value `div` 2) $
                 setCurrentProgramRegister r2 (value `div` 2) vm
            result = registerOperation (+) [Register r1, Register r2, Register r3] vm'

        getCurrentProgramRegister r3 result `shouldBe` value

      it "sub two registers and st the result in a third" $ do
        let vm' = setCurrentProgramRegister r1 (value * 2) $
                 setCurrentProgramRegister r2 value vm
            result = registerOperation (-) [Register r1, Register r2, Register r3] vm'

        getCurrentProgramRegister r3 result `shouldBe` value

-- FIXME : complete
--   describe "registerCarryOperation" $ do
--      testRegisterCarryOperation f vm carry = do
--        let dstRegisterNbr = 3
--
--        it "direct, indirect, register" $ do
--          let params = [Direct, Indirect, Register]
--              result = registerCarryOperation f params vm
--  
--          getCurrentProgramRegister dstRegisterNbr result `shouldBe` value
--
--        it "indirect, direct, register" $ do
--          let params = [Indirect, Direct, Register]
--              result = registerCarryOperation f params vm
--
--          getCurrentProgramRegister dstRegisterNbr result `shouldBe` value
--
--        it "register, register, register" $ do
--          let params = [Register, Register, Register]
--              result = registerCarryOperation f params vm
--
--          getCurrentProgramRegister dstRegisterNbr result `shouldBe` value
--
--        it "direct, register, register" $ do
--          let params = [Direct, Register, Register]
--              result = registerCarryOperation f params vm
--
--          getCurrentProgramRegister dstRegisterNbr result `shouldBe` value
--
--        it "register, direct, register" $ do
--          let params = [Register, Direct, Register]
--              result = registerCarryOperation f params vm
--
--          getCurrentProgramRegister dstRegisterNbr result `shouldBe` value
--
--        it "indirect, register, register" $ do
--          let params = [Indirect, Register, Register]
--              result = registerCarryOperation f params vm
--
--          getCurrentProgramRegister dstRegisterNbr result `shouldBe` value
--
--        it "register, indirect, register" $ do
--          let params = [Register, Indirect, Register]
--              result = registerCarryOperation f params vm
--
--          getCurrentProgramRegister dstRegisterNbr result `shouldBe` value
--
--      describe "and" $ do
--        it "and content of two locations, st result in a register, carry == False" $ do
--          testRegisterCarryOperation (.&.) vm False
--
--        it "and content of two locations, st result in a register, carry == True" $ do
--          testRegisterCarryOperation (.&.) vm True
--
--      describe "or" $ do
--        it "or content of two locations, st result in a register, carry == False" $ do
--          testRegisterCarryOperation (.|.) vm False
--
--        it "or content of two locations, st result in a register, carry == True" $ do
--          testRegisterCarryOperation (.|.) vm True
--
--      describe "xor" $ do
--        it "xor content of two locations, st result in a register, carry == False" $ do
--          testRegisterCarryOperation (Data.Bits.xor) vm False
--
--        it "xor content of two locations, st result in a register, carry == True" $ do
--          testRegisterCarryOperation (Data.Bits.xor) vm True

   describe "signedJmp" $ do
    it "increment the origin as the jmp offset is < size of a short" $ do
      let origin = 12
          offset = 24
          result = origin + fromIntegral offset

      signedJmp origin offset `shouldBe` result

    it "decrement the origin as the jmp offset is > size of a short" $ do
      let origin = 24
          offset = 65524 -- max Data.Word16 - 12 + 1 (for zero)
          result = origin - 12

      signedJmp origin offset `shouldBe` result

   describe "zjmp" $ do
    it "does not do anything as the carry is == True " $ do
      let vm' = setCurrentProgramCarry True vm
          offset = 12
          params = [Direct offset]

      pc (getCurrentProgram $ zjmp params vm') `shouldBe` pc (getCurrentProgram vm')

    it "increment the origin as the jmp offset is < size of a short" $ do
      let origin = 12
          vm' = setCurrentProgramPc origin $ setCurrentProgramCarry False vm
          offset = 24
          result = origin + offset
          params = [Direct offset]

      pc (getCurrentProgram $ zjmp params vm') `shouldBe` result

    it "decrement the origin as the jmp offset is > size of a short" $ do
      let origin = 24
          vm' = setCurrentProgramPc origin $ setCurrentProgramCarry False vm
          offset = 65524 -- max Data.Word16 - 12
          result = origin - 12
          params = [Direct offset]

      pc (getCurrentProgram $ zjmp params vm') `shouldBe` result

-- FIXME : complete
--   describe "ldi_" $ do
--    let registerNbr = 1
--        value = 42
--
--    testLdi_ f carry = $do
--      it "register, register, register" $ do
--        let params = [Register, Register, Register RegisterNbr]
--            result = ldi_ f params vm
--
--        getCurrentProgramRegister registerNbr result `shouldBe` value
--
--      it "direct, register, register" $ do
--        let params = [Direct, Register, Register RegisterNbr]
--            result = ldi_ f params vm
--
--        getCurrentProgramRegister registerNbr result `shouldBe` value
--
--      it "indirect, register, register" $ do
--        let params = [Indirect, Register, Register RegisterNbr]
--            result = ldi_ f params vm
--
--        getCurrentProgramRegister registerNbr result `shouldBe` value
--
--      it "register, direct, register" $ do
--        let params = [Register, Direct, Register RegisterNbr]
--            result = ldi_ f params vm
--
--        getCurrentProgramRegister registerNbr result `shouldBe` value
--
--      it "direct, direct, register" $ do
--        let params = [Direct, Direct, Register RegisterNbr]
--            result = ldi_ f params vm
--
--        getCurrentProgramRegister registerNbr result `shouldBe` value
--
--      it "indirect, direct, register" $ do
--        let params = [Indirect, Direct, Register RegisterNbr]
--            result = ldi_ f params vm
--
--        getCurrentProgramRegister registerNbr result `shouldBe` value
--
--    describe "ldi" $ do
--      it "" $ do
--        testLdi_ modIdxMod False
--
--      it "" $ do
--        testLdi_ modIdxMod True
--
--    describe "lldi" $ do
--      it "" $ do
--        testLdi_ id False
--
--      it "" $ do
--        testLdi_ id True
--
---- FIXME : complete
--   describe "sti_" $ do
--    registerNbr = 1
--
--    testSti = $ do
--      it "register, register, register" $ do
--        getValueFromMemory
--
--      it "register, direct, register" $ do
--        getValueFromMemory
--
--      it "register, indirect, register" $ do
--        getValueFromMemory
--
--      it "register, register, direct" $ do
--        getValueFromMemory
--
--      it "register, direct, direct" $ do
--        getValueFromMemory
--
--      it "register, indirect, direct" $ do
--        getValueFromMemory
--
--    describe "sti" $ do

   describe "fork_" $ do
    it "copies the current program and execute it at specified offset" $ do
      let offset = 4
          params = [Direct offset]
          program = getCurrentProgram vm
          vm' = fork_ id params vm

      length (programs vm') `shouldBe` 3
      head (programs vm') `shouldBe` program
      last (programs vm') `shouldBe` program { pc = (getCurrentProgramPc vm) + offset }

--   describe "fork" $ do
--   describe "lfork" $ do
    -- FIXME : test that idxMod is applied for fork, not for fork

--   describe "aff" $ do
--    it "append a character to the program aff buffer" $ do
--      let aff 
