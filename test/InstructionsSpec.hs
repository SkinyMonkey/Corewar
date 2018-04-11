module InstructionsSpec where

import Test.Hspec
import Test.QuickCheck

import Vm.Instructions

testInstructionsSpec =
  describe "Instructions" $ do
    let vm = newVm Vm []

    describe "live" $ do 
      it "try to make a program live" $ do
        let instruction = Instruction 1 [(Direct 1)]
        live ()
