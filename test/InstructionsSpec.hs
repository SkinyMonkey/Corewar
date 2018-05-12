module InstructionsSpec where

import Test.Hspec
import Test.QuickCheck
import qualified Data.ByteString.Char8 as B8
import Data.Maybe

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
       championNbrs = [0, 1]
       championsNbr = length championNbrs
       vm = foldl (insertTestProgram championsNbr) baseVm championNbrs
       program = head (programs vm)

   describe "live" $ do 
    it "set a program as alive" $ do
      let program' = program { alive = False }
          params = [Direct 1]
          result = live params $ setCurrentProgram program' vm

      (alive $ getCurrentProgram $ fromJust result) `shouldBe` True

    describe "fails as the champion nbr is not in range" $ do
      it "fails as the champion Nbr is == 0" $ do
        let program' = program { alive = False }
            params = [Direct 0]
            result = live params $ setCurrentProgram program' vm

        result `shouldBe` Nothing

      it "fails as the champion Nbr is > championsNbr" $ do
        let program' = program { alive = False }
            params = [Direct (fromIntegral championsNbr + 3)]
            result = live params $ setCurrentProgram program' vm

        result `shouldBe` Nothing

    describe "fails as the parameter is not accepted" $ do
      it "fails as the parameter is an indirect" $ do
        let params = [Indirect 1]
            result = live params vm

        result `shouldBe` Nothing

      it "fails as the parameter is a register" $ do
        let params = [Register 1]
            result = live params vm

        result `shouldBe` Nothing

   describe "ld" $ do
      -- FIXME: in this test
      -- check register value
      -- check carry value
      -- FIXME : test these cases:
      -- invalidRegister
      -- params = [Indirect 1]
      -- carry = False
--    rightTest params = do
--      let result = ld params vm
--      getCurrentProgramRegister vm

--    it "loads a value from memory into a register based on a direct" $ do
--      print "lelz"

--    it "loads a value from memory into a register based on an indirect" $ do
--      ;

    describe "fails as the parameters are not accepted" $ do
      it "fails as the first parameter is a register" $ do
        let params = [Register 1, Register 1]
            result = ld params vm

        result `shouldBe` Nothing
