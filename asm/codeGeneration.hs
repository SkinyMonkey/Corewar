module CodeGeneration (
  generateCode
) where

import ChampionData

--serializeDirect value = do
--  putWord32be value
--
--serializeRegister value = do
--  putWord32be value
--
--serializeIndirect value = do
--  putWord16be value
--
--serializeOpCode opCode = do
--  putWord8 opCode
--
--serializeInstructionCode instructionCode = do
--  putWord8 instructionCode
--
--serializeParameter :: Put
--serializeParameter = do
--    serializeDirect 1
--    serializeIndirect 2
-- 
--putSerialize code = do
--  B.appendFile "tests/res.cor" $ B.concat $ BL.toChunks $ runPut (serializeOpCode code)
----  B.appendFile "tests/res.cor" $ B.concat $ BL.toChunks $ runPut serializeParameter
--
---- FIXME : add encoding based on arg type
--encodeDescription (p1:p2:p3:p4:[]) = "p4"
--encodeDescription (p1:p2:p3:[]) = "p3"
--encodeDescription (p1:p2:[]) = "p2"
--encodeDescription (p1:[]) = "p1"
--encodeDescription [] = "no p"

generateInstructions cd = (True, cd)

finished (True, cd) = putStrLn $ "Generation complete for " ++ getFileName cd
finished (False, cd) = putStrLn $ "Generation failed for " ++ getFileName cd

generateCode cd =
  finished $ generateInstructions cd
