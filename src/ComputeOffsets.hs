module ComputeOffsets where

import Data.Word
import qualified Data.Map as Map

import Op
import ChampionData

-- TODO : for every arg in instruction
--        put instruction offset ( + argSize?) into instructions
--        -> instructionOffset - labelOffset
--        transform into an indirect? with (Indirect, show offset)
--        -- getLabelOffset championData, label
computeArgLabelOffset :: ChampionData -> ArgType String -> ArgType String
computeArgLabelOffset championData arg =
  case arg of
     Label labelName -> Indirect $ show $ getLabelOffset championData labelName
     _ -> arg

computeArgsLabelOffset :: ChampionData -> (Word8, [ArgType String], Offset) -> (Word8, [ArgType String], Offset)
computeArgsLabelOffset championData (op, args, offset) = (op, map (computeArgLabelOffset championData) args, offset)

computeLabelAdressing :: ChampionData -> ChampionData
computeLabelAdressing championData =
  let instructions = getInstructions championData
      newInstructions = map (computeArgsLabelOffset championData) instructions
  in setInstructions championData newInstructions
