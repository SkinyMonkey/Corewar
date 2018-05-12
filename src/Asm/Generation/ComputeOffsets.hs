module Asm.Generation.ComputeOffsets where

import Data.Word
import qualified Data.Map as Map

import Op
import Asm.ChampionData

-- Labels are transformed into Indirects
computeArgLabelOffset :: ChampionData -> Offset -> Parameter -> Parameter
computeArgLabelOffset championData instructionOffset parameter =
  case parameter of
     Label labelName -> 
      let labelOffset = getLabelOffset championData labelName
      in Direct $ labelOffset - instructionOffset -- FIXME ? invert?
     _ -> parameter

computeArgsLabelOffset :: ChampionData -> Instruction  -> EvaluatedInstruction
computeArgsLabelOffset championData (op, parameters, offset) =
  (op, map (computeArgLabelOffset championData offset) parameters)

computeLabelAdressing :: ChampionData -> [EvaluatedInstruction]
computeLabelAdressing championData =
  let instructions = getInstructions championData
  in map (computeArgsLabelOffset championData) instructions
