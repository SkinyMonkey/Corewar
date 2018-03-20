module CheckArgsSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Op
import CheckArgs
import ChampionData

testCheckArgs =
  describe "CheckArgs" $ do
    let failed = Nothing
        worked x = Just x
        emptyResult = worked []
        championData = newChampionData "test"

    describe "isRegister" $ do 
      it "should check a register arg" $ do
        isRegister "r1" [] `shouldBe` worked [Register "1"]

      it "should fail" $ do
        isRegister "rx" [] `shouldBe` failed
        isRegister "r" [] `shouldBe` failed
    
    describe "isLabel" $ do
      it "should check a label arg" $ do
        isLabel ":label" [] `shouldBe` worked [Label "label"]
        isLabel ":l1" [] `shouldBe` worked [Label "l1"]
        
      it "should fail" $ do
        isLabel "label" [] `shouldBe` failed
        isLabel ":" [] `shouldBe` failed

    describe "isDirect" $ do
      it "should check a direct arg" $ do
        isDirect "%123" [] `shouldBe` worked [Direct "123"]
        isDirect "%:ok" [] `shouldBe` worked [Label "ok"]
 
      it "should fail" $ do
        isDirect "123" [] `shouldBe` failed
        isDirect "%ok" [] `shouldBe` failed

    describe "isIndirect" $ do
      it "should check an indirect arg" $ do
        isIndirect "123" [] `shouldBe` worked [Indirect "123"]
        isIndirect ":ok" [] `shouldBe` worked [Label "ok"]
 
      it "should fail" $ do
        isIndirect "%123" [] `shouldBe` failed
        isIndirect ":123" [] `shouldBe` failed

    describe "checkArgType'" $ do
      it "should check an arg based on an argType" $ do
        checkArgType' "r1" register [] `shouldBe` [Register "1"]
        checkArgType' "1" indirect [] `shouldBe` [Indirect "1"]
        checkArgType' ":id" indirect [] `shouldBe` [Label "id"]
        checkArgType' "%1" direct [] `shouldBe` [Direct "1"]
        checkArgType' "%:id" direct [] `shouldBe` [Label "id"]

      it "should return an empty list" $ do
        checkArgType' "r" register [] `shouldBe` []

    describe "checkArgType" $ do
      let argTypes = [direct, register]
          argTypesNbr = getNbrArgs $ byMnemonic "live"

      it "should match the arg with a type based on of the argTypes" $ do
        checkArgType argTypesNbr (argTypes, "r1") [] `shouldBe` [Register "1"]

        let argTypes' = [indirect, register]
            argTypesNbr' = getNbrArgs $ byMnemonic "st"
        checkArgType argTypesNbr' (argTypes', "19") [] `shouldBe` [Indirect "19"]

      it "should fail to match the arg with a type" $ do
        evaluate (checkArgType argTypesNbr (argTypes, "r") []) `shouldThrow` errorCall "Argument did not match any authorized types :\ntoken : \"r\" -> valid possible argTypes [Direct (),Register ()] -> found argType []"

    describe "rightArgsNbr" $ do
      -- TODO : check for every mnemonic
      let op = byMnemonic "live"
      it "should check for the argument number" $ do
        rightArgsNbr op ["r1"] championData `shouldBe` True

      it "should fail to check for the argument number" $ do
        evaluate (rightArgsNbr op ["r1", "r2"] championData) `shouldThrow` errorCall "Bad # of args for mnemonic \"live\": 2 instead of 1 in \"\""
        evaluate (rightArgsNbr op [] championData) `shouldThrow` errorCall "Bad # of args for mnemonic \"live\": 0 instead of 1 in \"\""
