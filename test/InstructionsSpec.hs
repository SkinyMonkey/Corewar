module InstructionsSpec where

import Test.Hspec
import Test.QuickCheck
import Data.ByteString.Char8
import Data.Maybe

import Vm.Vm
import Vm.Instructions

testInstructions =
  describe "Instructions" $ do
   let program = newProgram 0 "test" 0 $ pack ""
       vm = newVm [program]

   describe "live" $ do 
     -- FIXME : create a program from scratch with alive = False
     it "try to make a program live" $ do
       let params = [PDirect 0]
           result = live params vm
       print $ alive $ getCurrentProgram $ fromJust result

     -- FIXME : TO TEST
     -- playerNbr < 0 || playerNbr > length programs
     -- params = [PIndirect 1]

  describe "ld" $ do
    it "loads a value from memory into a register" $ do
      let params = [PDirect 0]
          result = ld params vm

      -- FIXME:
      -- check register value
      -- check carry value

      -- FIXME : TO TEST
      -- invalidRegister
      -- params = [PIndirect 1]
      -- carry = False
