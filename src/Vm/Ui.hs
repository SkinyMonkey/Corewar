module Vm.Ui where

import Numeric (showHex, showIntAtBase)
import qualified Data.ByteString as B
import Data.ByteString.Internal
import Data.Word
import Data.Char
import Data.Bits
import Data.List
import UI.NCurses

import Debug.Trace
import Vm.Vm
import Op
import Utils

-- Definitions

-- Game
gameColNumber = 59
gameRowNumber = 39
gameCellNumber = gameColNumber * gameRowNumber

-- FIXME : import from Op.hs
--memorySize = gameCellNumber

-- Visual
bottomPaneSize = 5
topPaneSize rows = rows - bottomPaneSize - borderSize

borderSize = 1
leftMargin = borderSize + 1
rightMargin = borderSize + 2
bottomMargin = bottomPaneSize
cellSize = fromIntegral $ length "00 "

-- Colors
color1 = ColorGreen
color2 = ColorBlue
color3 = ColorRed
color4 = ColorCyan

declareColors = do
  -- defineColor ColorWhite 300 300 300
  -- defineColor 42 128 128 128
  -- newColorID (ColorBlack + 8) ColorBlack 42

  -- foreground background Id
  sequence [
    newColorID ColorWhite ColorBlack 1, -- default color
    newColorID color1 ColorBlack 2, -- player 1
    newColorID color2 ColorBlack 3, -- player 2
    newColorID color3 ColorBlack 4, -- player 3
    newColorID color4 ColorBlack 5, -- player 4

    newColorID color1 ColorWhite (2 .|. 8),
    newColorID color2 ColorWhite (3 .|. 8),
    newColorID color3 ColorWhite (4 .|. 8),
    newColorID color4 ColorWhite (5 .|. 8),

    newColorID color1 ColorWhite (2 .|. 16),
    newColorID color2 ColorWhite (3 .|. 16),
    newColorID color3 ColorWhite (4 .|. 16),
    newColorID color4 ColorWhite (5 .|. 16),

    newColorID ColorWhite ColorWhite (1 .|. 16),
    newColorID ColorBlack ColorWhite (1 .|. 8)]

-- Implementation

-- Applies a side effect on a list
-- and fold to compose them together
-- sideEffects f xs = foldl1 (>>) $ map f xs
-- -> equal to mapM

-- FIXME : EIP could be a special player, with a value of 0 or something like that?
-- FIXME : setColor
drawOneCell defaultColor rowIndex (el, playerColor, cellCol) = do
  moveCursor rowIndex (cellCol + leftMargin)
  setColor playerColor
  drawString $ (if el < 16 then "0" else "") ++ (showHex el "")
  setColor defaultColor
  drawString " "

colorRow offset pcs colors rowLength row = 
  let isPc offset = offset `elem` pcs
      rowOffsets = [fromIntegral offset.. fromIntegral (offset + fromIntegral rowLength)] :: [Offset];

  in [ colors !! if isPc offset then (fromIntegral value) + 4 else fromIntegral value
     | (value, offset) <- zip row rowOffsets ]

drawRow :: [Integer] -> [Offset] -> [ColorID] -> [Word8] -> [Word8] -> Integer -> Integer -> Update ()
drawRow cellCols pcs colors memory graphicMemory rowIndex rowLength =
  let offset = fromIntegral $ (rowIndex - 1) * rowLength
      defaultColor = colors !! 0
      getRow = slice offset (offset + fromIntegral rowLength)

      gameRow = getRow memory
      graphicRow = colorRow offset pcs colors rowLength $ getRow graphicMemory

      row = zip3 gameRow graphicRow cellCols
  in  mapM_ (drawOneCell defaultColor rowIndex) row

drawMemory pcs colors rows rowLength memory graphicMemory =
 let cellCols = [ x | x <- [0..(rowLength * cellSize) - 1], x `mod` cellSize == 0];
     drawMemory' rowIndex = drawRow cellCols pcs colors memory graphicMemory rowIndex rowLength
 in mapM_ drawMemory' [borderSize..(topPaneSize rows)]

-- FIXME : create one pad for the game, one for the stats?
-- have two functions,
-- one to update the game pad
-- one for the stats pad
-- size of pad based on screenSize and ratio
renderGame vm = do
  w <- defaultWindow
  colors <- declareColors
  updateWindow w $ do
    (rows, cols) <- windowSize
    let rowLength = (fromIntegral $ cols `div` cellSize) - 1
        unpackedMemory = B.unpack $ memory vm
        unpackedGraphicMemory = B.unpack $ graphicMemory vm
        pcs = map (\p -> pc p) (programs vm)
    drawMemory pcs colors rows rowLength unpackedMemory unpackedGraphicMemory
  render

cyclesLeftText = "cycles left : "
aliveText = "alive : "
registerText = "registers : "

drawProgramStat :: Int -> Program -> Update ()
drawProgramStat rows program = do
  let championNbr = number program
      row = ((fromIntegral rows) - 4 + (fromIntegral championNbr))

  -- champion number
  let championNumberString = "#" ++ show championNbr
  moveCursor row leftMargin
  drawString championNumberString

  -- cycles left
  let cyclesLeftMargin = leftMargin + (fromIntegral $ length championNumberString) + 1
      cycles = cyclesLeft program
      cyclesLeftString = cyclesLeftText ++ (lpad 3 $ show cycles)
  moveCursor row cyclesLeftMargin
  drawString cyclesLeftString

  -- alive 
  let aliveMargin = cyclesLeftMargin + (fromIntegral $ length cyclesLeftString) + 3
      aliveString = aliveText ++ show (alive program)
  moveCursor row aliveMargin
  drawString aliveString

  -- registers
  let registerMargin = aliveMargin + (fromIntegral $ length aliveString) + 3
      registerString = registerText ++ show (registers program)
  moveCursor row registerMargin
  drawString registerString

renderStats vm = do
  w <- defaultWindow
  updateWindow w $ do
    (rows, _) <- windowSize
    mapM_ (drawProgramStat $ fromIntegral rows) (programs vm)
  render

-- VM

drawBottomPane = do
  (rows, cols) <- windowSize
  moveCursor (rows - bottomMargin) 1
  drawLineH Nothing (cols - rightMargin)

drawWindowBorder = drawBox Nothing Nothing

renderFrame = do
    w <- defaultWindow
    updateWindow w $ do
      clear
      drawWindowBorder
      drawBottomPane
    render

renderVm vm gameLoop = do
  runCurses $ do
    setEcho False

    renderFrame
    gameLoop vm 0

-- treat event in separate thread?
-- encapsulate Vm into a GameState record?
-- to keep info about the rendering?
--
-- treatEvent :: Vm -> Vm
-- treatEvent vm = do
--   event <- getEvent
--   case event of
--     Space -> vm { paused = not $ isPaused vm }
--     EventResized -> renderFrame
--     _ -> 
