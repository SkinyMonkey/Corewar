module ParseAsmSpec where

import Test.Hspec
import Control.Exception (evaluate)

import Op
import ChampionData
import ParseAsm

-- TODO : finish and use
-- goal is to 
--  test parseInstruction for each possible instruction
--  test checkArgTypes for each possible instruction ?
--  test checkArgNbr for each possible instruction ?
-- generateInstruction = ;

testParseAsm =
  describe "ParseAsm" $ do
    let failed x = Left x
        worked x = Right x
        championData = newChampionData "test"

    describe "dropComments" $ do
      it "should drop all the words after a comment char" $ do
        dropComments ["#", "ok", "ok"] `shouldBe` []
        dropComments [";", "ok", "ok"] `shouldBe` []
        dropComments ["before", ";", "ok", "ok"] `shouldBe` [ "before" ]
        dropComments ["before", "#", "ok", "ok"] `shouldBe` [ "before" ]

    describe "splitOnCommas" $ do
      it "should split a line on commas" $ do
        splitOnCommas ["live",  "arg1,arg2,arg3"] `shouldBe` [ "live", "arg1", "arg2", "arg3" ]

    describe "parseMetadata, parse a line to extract its metadata" $ do
      it "should parse a Metadata field but not add it" $ do
        parseMetadata (words ".test \"OK\"") championData `shouldBe` worked championData

      it "should fail to parse a Metadata field" $ do
        let errorMsg = "Malformed metadata"
        parseMetadata (words "test \"\"")  championData `shouldBe` failed errorMsg 
        parseMetadata (words ".test \"\"") championData `shouldBe` failed errorMsg
        parseMetadata (words ".test OK\"") championData `shouldBe` failed errorMsg
        parseMetadata (words ".test \"OK") championData `shouldBe` failed errorMsg
 
      it "should parse and add the .name Metadata field" $ do
        let championData' = addMetadata championData "name" "OK"

        parseMetadata (words ".name \"OK\"") championData `shouldBe` worked championData'

    describe "parseLabel, parse a line to extract its label" $ do
      it "should parse a label and add it to championData" $ do
        let championData' = addLabel championData "ok"
        parseLabel "ok:" championData `shouldBe` worked championData'

      it "should fail to parse a label as input is malformed" $ do
        let errorMsg = "Malformed label"
        parseLabel "ok" championData `shouldBe` failed errorMsg
        parseLabel ":ok" championData `shouldBe` failed errorMsg

    describe "parseOp, parse an op and its arguments" $ do
      it "should parse an op and its args and add it to a championData" $ do
        let op = byMnemonic "live"
            instructionsChampionData = addInstruction championData op [Direct "1"]
            line = words "live %1"
        parseOp line championData `shouldBe` worked instructionsChampionData

      -- FIXME : replace by a test with failed
      it "should fail to parse an op as the mnemonic wasnt found" $ do
        let line = words "mv"

        evaluate (parseOp line championData) `shouldThrow` errorCall "Unknown mnemonic found : mv"

    describe "parseInstruction, parse a line by dispatching it to the right rules" $ do
      it "should parse the metadata and add it to the championData header" $ do
        let metadataChampionData = addMetadata championData "name" "valid string"
            line = words ".name \"valid string\""
        parseInstruction line championData `shouldBe` worked metadataChampionData

      it "should parse the label and add it to the championData labels" $ do
        let labelChampionData = addLabel championData "label"
            line = words "label:"
        parseInstruction line championData `shouldBe` worked labelChampionData
 
      it "should parse the instructions and add them to the championData instructions" $ do
        let op = byMnemonic "live"
            instructionsChampionData = addInstruction championData op [Direct "1"]
            line = words "live %1"
        parseInstruction line championData `shouldBe` worked instructionsChampionData

      it "should parse the label and instructions and add them to the championData labels and instructions" $ do
        let op = byMnemonic "live"
            labelChampionData = addLabel championData "label"
            instructionsChampionData = addInstruction labelChampionData op [Direct "1"]
            line = words "label: live %1"
        parseInstruction line championData `shouldBe` worked instructionsChampionData

    describe "parseLine, splits a line if its not null or empty, cleans it from comments, maintains the line and line nbr and pass it to parseInstruction" $ do
      let testParseLine line championDataResult = parseLine (Just championData) line `shouldBe` Just (incLineNbr $ setCurrentLine championDataResult line)

      it "should ignore the comment but increment the line nbr" $ do
        testParseLine "# all this is ignored" championData
        testParseLine "; all this is ignored" championData

      it "should do nothing but maintain the line and lineNbr as the line is empty" $ do
        testParseLine "" championData
        testParseLine " " championData

      it "should parse and add an instruction to championData" $ do
        let op = byMnemonic "live"
            line = "live %1"
            instructionsChampionData = incLineNbr $ setCurrentLine (addInstruction championData op [Direct "1"]) line
        parseLine (Just championData) line `shouldBe` Just instructionsChampionData

      it "should parse and add an instruction to championData but ignore the comments" $ do
        let op = byMnemonic "live"
            hashLineChampionData = addInstruction championData op [Direct "1"]
        testParseLine "live %1 # comment" hashLineChampionData

        let semiColChampionData = addInstruction championData op [Direct "1"]
        testParseLine "live %1 ; comment" semiColChampionData

        let championData' = addInstruction championData op [Direct "1"]
        testParseLine "live %1 # %2" championData'

    -- FIXME : finish, see TODO
