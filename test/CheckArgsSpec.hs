module CheckArgsSpec where

import Test.Hspec
import Op
import CheckArgs
import ChampionData
import Control.Exception (evaluate)

testCheckArgs =
  describe "CheckArgs" $ do
    let failed = Nothing
        worked x = Just x
        emptyResult = worked []
        championData = newChampionData "test"

    it "should check a register arg" $ do
      isRegister "r1" [] `shouldBe` worked [Register "1"]

      isRegister "rx" [] `shouldBe` failed
      isRegister "r" [] `shouldBe` failed

    it "should check a label arg" $ do
      isLabel ":label" [] `shouldBe` worked [Label "label"]
      isLabel ":l1" [] `shouldBe` worked [Label "l1"]
      
      isLabel "label" [] `shouldBe` failed
      isLabel ":" [] `shouldBe` failed

    it "should check a direct arg" $ do
      isDirect "%123" [] `shouldBe` worked [Direct "123"]
      isDirect "%:ok" [] `shouldBe` worked [Label "ok"]
      
      isDirect "123" [] `shouldBe` failed
      isDirect "%ok" [] `shouldBe` failed

    it "should check an indirect arg" $ do
      isIndirect "123" [] `shouldBe` worked [Indirect "123"]
      isIndirect ":ok" [] `shouldBe` worked [Label "ok"]
      
      isIndirect "%123" [] `shouldBe` failed
      isIndirect ":123" [] `shouldBe` failed

    it "should check an arg based on an argType" $ do
      let argType = register
      checkArgType' "r1" argType [] `shouldBe` [Register "1"]

      checkArgType' "r" argType [] `shouldBe` []

    it "should match the arg with on of the argTypes" $ do
      let argTypes = [direct, register]
          argTypesNbr = getNbrArgs $ byMnemonic "live"
      checkArgType argTypesNbr (argTypes, "r1") [] `shouldBe` [Register "1"]
      
      evaluate (checkArgType argTypesNbr (argTypes, "r") []) `shouldThrow` errorCall "Argument did not match any authorized types : \"r\" -> [Direct (),Register ()] -> []"

    it "should check for the argument number" $ do
      let op = byMnemonic "live"
      rightArgsNbr op ["r1"] championData `shouldBe` True
      
      evaluate (rightArgsNbr op ["r1", "r2"] championData) `shouldThrow` errorCall "Bad # of args for mnemonic \"live\": 2 instead of 1 in \"\""
      evaluate (rightArgsNbr op [] championData) `shouldThrow` errorCall "Bad # of args for mnemonic \"live\": 0 instead of 1 in \"\""
