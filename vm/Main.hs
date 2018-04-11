import System.Environment (getArgs)

import Debug.Trace

-- load
--  read file
--  get program code after header
--  create program into vm + load at right offset -> loadProgram in Vm.hs?

-- executeInstruction instruction params vm
-- if instruction > 0 && instruction < length instructionTable
-- then let instructionHandler = instructionTable !! instruction
--      in instructionHandler params vm
-- else Nothing

-- incrementPcByOpCode instruction paramCode
-- if instruction has paramCode
-- then if validOpCode paramCode
--      then incrementPc (paramCodeSize paramCode)
--      else incrementPc 1
-- else incrementPc 2 -- instruction + paramCode

-- execute vm
-- vm ' = if cyclesLeft program == 0
--        then let instruction paramCode args = get instruction and args from memory at pc offset
--                  vm' = execute instruction with args on vm
--             in incrementPcByOpCode instruction paramCode (fromMaybe vm vm')
--        else vm
-- decrementCycleLeft (getCurrentProgram vm')

-- FIXME : at MEM_CYCLE checks
--         insert a render VM loop?
-- play vm
--  foldl execute vm (programs vm)

-- FIXME : how to know which user owns what?
--         set user number in graphicMemory
--         everytime something is written in memory
--         -> in setMemoryByCurrentProgramPc
-- renderMemory vm
-- for value in memory
--  color = if value == 0
--          then white
--          else getPlayerColor graphicMemory
--  result = color value
--  background color = if idx === currentProgram pc
--                     then light color
--                     else black
--  if x === screenLimit
--    x = 0
--    y += 1
--  print result x y

-- firstRendering :: Screen ()

-- renderStats :: Screen ()
-- renderStats = do

-- rendering :: Screen ()
-- rendering = do
--  renderStats vm
--  renderMemory vm

-- gameLoop vm
--  vm' = play vm
--  rendering vm
--  if nbrAlive vm > 0
--  then Graphics.UI.SDL.Time.delay 30
--       gameLoop vm
--  else return vm

main :: IO ()
main = do
  args <- getArgs
  -- vm = foldl load (new Vm) args
  -- firstRendering vm >>= gameLoop screen vm
  putStrLn "End VM"
