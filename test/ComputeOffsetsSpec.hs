module ComputeOffsetsSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Op
import ComputeOffsets
import ChampionData

testComputeOffsets =
  describe "ComputeOffsets" $ do
    let failed = Nothing
        worked x = Just x
        emptyResult = worked []
        championData = newChampionData "test"

    describe "A" $ do 
      it "B" $ do
        let newChampionData = addLabel (setByteCounter championData 15) "live"
        computeArgLabelOffset newChampionData 0 (Label "live") `shouldBe` Indirect 15
