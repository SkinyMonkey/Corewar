module CheckArgsSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Op
import Asm.ChampionData
import Asm.Parsing.CheckArgs

testCheckArgs =
  describe "CheckArgs" $ do
    let failed = Nothing
        worked x = Just x
        emptyResult = worked []
        championData = newChampionData "test"

    describe "isRegister" $ do 
      it "should check a register arg" $ do
        isRegister "r1" `shouldBe` worked (Register 1)
 
      it "should fail" $ do
        isRegister "rx" `shouldBe` failed
        isRegister "r" `shouldBe` failed
    
    describe "isLabel" $ do
      it "should check a label arg" $ do
        isLabel ":label" `shouldBe` worked (Label "label")
        isLabel ":l1" `shouldBe` worked (Label "l1")
        
      it "should fail" $ do
        isLabel "label" `shouldBe` failed
        isLabel ":" `shouldBe` failed
 
    describe "isDirect" $ do
      it "should check a direct arg" $ do
        isDirect "%123" `shouldBe` worked (Direct 123)
        isDirect "%:ok" `shouldBe` worked (Label "ok")
 
      it "should fail" $ do
        isDirect "123" `shouldBe` failed
        isDirect "%ok" `shouldBe` failed

    describe "isIndirect" $ do
      it "should check an indirect arg" $ do
        isIndirect "123" `shouldBe` worked (Indirect 123)
        isIndirect ":ok" `shouldBe` worked (Label "ok")
 
      it "should fail" $ do
        isIndirect "%123" `shouldBe` failed
        isIndirect ":123" `shouldBe` failed

    describe "checkParameter'" $ do
      it "should check an arg based on an argType" $ do
        checkParameter' "r1" [] register `shouldBe` [Register 1]
        checkParameter' "1" [] indirect `shouldBe` [Indirect 1]
        checkParameter' ":id" [] indirect `shouldBe` [Label "id"]
        checkParameter' "%1" [] direct `shouldBe` [Direct 1]
        checkParameter' "%:id" [] direct `shouldBe` [Label "id"]
 
      it "should return an empty list" $ do
        checkParameter' "r" [] register `shouldBe` []
 
    describe "checkParameter" $ do
      it "should match the arg with a type based on of the argTypes" $ do
        let argTypes = [direct, register]
            argTypesNbr = getNbrArgs $ byMnemonic "live"
        checkParameter argTypesNbr championData ("", []) (argTypes, "r1") `shouldBe` ("", [Register 1])
 
        let argTypes' = [indirect, register]
            argTypesNbr' = getNbrArgs $ byMnemonic "st"
        checkParameter argTypesNbr' championData ("", []) (argTypes', "19") `shouldBe` ("", [Indirect 19])

      it "should fail to match the arg with a type" $ do
        let argTypes = [direct, register]
            argTypesNbr = getNbrArgs $ byMnemonic "live"
        checkParameter argTypesNbr championData ("", []) (argTypes, "r") `shouldBe` ("\nArgument did not match any authorized types :\ntoken : \"r\" -> valid possible argTypes [Direct 0,Register 0] -> found argType [] -> result [] at line 0",[])
