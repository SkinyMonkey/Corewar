import System.Environment (getArgs)
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word
import Data.Binary.Get
import Data.Maybe

import Debug.Trace

import Utils
import Op
import Vm.Vm
import Vm.UnpackInstruction
import Vm.Instructions
import Vm.Ui

-- FIXME : import from somewhere?
refreshDelay = 100000

loadProgram :: Int -> Vm -> (Int, B.ByteString) -> Vm
loadProgram championsNbr vm (championNumber, fileContent) =
  insertProgram championsNbr championNumber fileContent vm

loadPrograms :: Vm -> [B.ByteString] -> Vm
loadPrograms vm fileContents =
  let championsNbr = length fileContents
      championNumbers = [0..championsNbr - 1]
  in foldl (loadProgram championsNbr) vm $ zip championNumbers fileContents

executeInstruction :: Instruction -> Vm -> Vm
executeInstruction (Instruction i p size cycles) vm =
  if i > 0 && fromIntegral i < length instructionTable
  then let instructionHandler = instructionTable !! ((fromIntegral i) - 1)
           vm' = instructionHandler p vm
        in (incrementCurrentProgramPc size . setCurrentProgramCycleLeft cycles) $ fromMaybe vm vm'
  else vm

-- FIXME : move to Vm/Vm.hs ???
getInstructionByCurrentPc :: Vm -> Instruction
getInstructionByCurrentPc vm =
  let offset = getCurrentProgramPc vm
      memory' = bslice (fromIntegral offset) ((fromIntegral offset) + 32) (memory vm)
  in runGet getInstruction $ BL.fromStrict memory'

execute :: Vm -> Program -> Vm
execute vm program =
  let vm' = setCurrentProgramNbr program vm
  in if cyclesLeft program == 0
     then let i = getInstructionByCurrentPc vm'
          in executeInstruction i vm'
     else decrementCurrentProgramCycleLeft vm'

-- FIXME : at MEM_CYCLE checks
--         set all alive to false!
play :: Vm -> Vm
play vm = foldl execute vm (programs vm)

gameLoop vm index = do
  renderGame vm
  renderStats vm

  let vm' = play vm
--  if nbrAlive vm > 0 || MEM_CYCLE == 0?
  if index < 200
  then do liftIO $ threadDelay refreshDelay
          gameLoop vm' (index + 1) -- vm
  else return vm -- vm
--          if gameState.alive
--          then gameLoop
--          else return

main :: IO ()
main = do
  args <- getArgs
  if length args >= 2 && length args <= 4
  then do
    fileContents <- mapM B.readFile $ args
    let vm = loadPrograms newVm fileContents
    vm' <- renderVm vm gameLoop
    print (programs vm')
    putStrLn "End VM"
  else putStrLn "usage: ./vm champion.cor [champion.cor]+"
