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

-- FIXME : import from VM?
refreshDelay = 100000

loadProgram :: Int -> Vm -> (Int, B.ByteString) -> Vm
loadProgram championsNbr vm (championNumber, fileContent) =
  insertProgram championsNbr championNumber fileContent vm

loadPrograms :: Vm -> [B.ByteString] -> Vm
loadPrograms vm fileContents =
  let championsNbr = length fileContents
      championNumbers = [0..championsNbr - 1]
  in foldl (loadProgram championsNbr) vm $ zip championNumbers fileContents

executeInstruction :: Vm -> Instruction -> Maybe Vm
executeInstruction vm (Instruction index p size cycles) =
  let handler = instructionTable !! index
      vm' = handler p vm
      -- FIXME : index `elem` leavePcIntact
      increment = if index == 0x08 then id else incrementCurrentProgramPc size
  in (increment . setCurrentProgramCycleLeft cycles) <$> Just vm'

getInstructionByCurrentPc :: Vm -> Maybe Instruction
getInstructionByCurrentPc vm =
  let offset = fromIntegral $ getCurrentProgramPc vm
      -- FIXME : circle buffer behavior
      -- FIXME : why 32? shouldn't it be an offset based on the instruction?
      offsetEnd = offset + 32 -- FIXME : why 32? seems arbitrary?
      memorySlice = BL.fromStrict $ bslice offset offsetEnd (memory vm)
      championsNbr = length $ programs vm
      getInstruction' = getInstruction championsNbr

  in runGet getInstruction' memorySlice

execute :: Vm -> Program -> Vm
execute vm program =
  let vm' = setCurrentProgramNbr program vm
  in if cyclesLeft program == 0
     then fromMaybe (incrementCurrentProgramPc 1 vm') (getInstructionByCurrentPc vm' >>= executeInstruction vm')
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
  if index < 10000
  then do liftIO $ threadDelay refreshDelay
          gameLoop vm' (index + 1) -- vm
  else return vm'
--          if gameState.alive
--          then gameLoop
--          else return

main :: IO ()
main = do
  args <- getArgs
  if length args >= 2 && length args <= 4
  then do
    fileContents <- mapM B.readFile args
    let vm = loadPrograms newVm fileContents
--    gameLoop vm 0 -- DEBUG (without ncurses)
    vm' <- renderVm vm gameLoop
    print (programs vm')
    putStrLn "End VM"
  else putStrLn "usage: ./vm champion.cor [champion.cor]+"
