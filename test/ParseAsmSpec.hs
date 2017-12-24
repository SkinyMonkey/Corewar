module ParseAsmSpec where

import Test.Hspec

import Op
import ChampionData
import ParseAsm

testParseAsm =
  describe "ParseAsm" $ do
    let failed = Nothing
        worked x = Just x
        championData = newChampionData "test"

--        emptyResult = worked []

    it "should parse a Metadata field but not add it" $ do
      parseMetadata (words ".test \"OK\"") championData `shouldBe` worked championData

      parseMetadata (words "test \"\"")  championData `shouldBe` failed
      parseMetadata (words ".test \"\"") championData `shouldBe` failed
      parseMetadata (words ".test OK\"") championData `shouldBe` failed
      parseMetadata (words ".test \"OK") championData `shouldBe` failed
 
    it "should parse and add the .name Metadata field" $ do
      let championData' = addMetadata championData "name" "OK"

      parseMetadata (words ".name \"OK\"") championData `shouldBe` worked championData'

    it "should parse a label and add it to championData" $ do
      let championData' = addLabel championData "ok"
      parseLabel "ok:" championData `shouldBe` worked championData'

      -- FIXME : change parseInstruction'
      --        -> parseLabel modified to work with test?
      parseLabel "ok" championData `shouldBe` failed
      parseLabel ":ok" championData `shouldBe` failed

--    it "should parse a line by dispatching it to the right rules" $ do
    -- FIXME : finish
    --  -- comment
    --  parseInstruction' ["#", "all this is ignored"]

    --  -- metadata
    --  parseInstruction' [".name", "\"valid string\""]

    --  -- label
    --  parseInstruction' ["label:"]
    --  parseInstruction' ["label:", "live", "%1"]

    -- FIXME : remove parseInstruction?
    --         it only words the line and remove empty ones
    --         we can filter the empty lines out in parseChampion
    --         rename parseInstruction'
    --
    --
    -- FIXME : see checkArgType too? for changes
