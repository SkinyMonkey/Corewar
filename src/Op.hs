module Op where

-- import Data.Bits
import Data.Word

-- FIXME : USE
-- Internals definitions
memSize = 2301 :: Int -- 8 * 1024 + 717 :: Int
idxMod = 512 :: Int
maxArgsNumber = 4 :: Int
regNumber = 16 :: Int
-- FIXME : move header length definition into op and use them
magicSize = 4
nameSize = 128
progSizeSize = 4
commentSize = 2048
headerSize = magicSize + nameSize + progSizeSize + commentSize
nameOffset = magicSize :: Int
progSizeOffset = nameOffset + nameSize :: Int
commentOffset = progSizeOffset + progSizeSize :: Int

-- Asm syntax
_commentChar = '#'
_labelChar = ':'
_directChar = '%'
_separatorChar = ','
_labelChars = "abcdefghijklmnopqrstuvwxyz_0123456789"
_nameCmdString = ".name"
_commentCmdString = ".comment"

-- Size for memory access
regSize = 1 :: Int
indSize = 2 :: Int
dirSize = 4 :: Int

type InstructionCode = Word8
type OpCode = Word8
type Offset = Word32
type RegisterNbr = Word8
type RegisterValue = Word8

-- Op arguments types

data Parameter = Register Word8 | Indirect Word16 | Direct Word32 | Label String deriving (Show, Eq)

register = Register 0
direct = Direct 0
indirect = Indirect 0

-- Param octet codage 

data Op = Op {
  mnemonic :: String,
  nbrArgs :: Int,
  argsType :: [[Parameter]],
  code :: Word8,
  nbrCycles :: Int,
  comment :: String
} deriving (Show)

getMnemonic :: Op -> String
getMnemonic = mnemonic

getNbrArgs :: Op -> Int
getNbrArgs = nbrArgs

getArgsTypes :: Op -> [[Parameter]]
getArgsTypes = argsType

getCode :: Op -> Word8
getCode = code

getNbrCycles :: Op -> Int
getNbrCycles = nbrCycles

getComment :: Op -> String
getComment = comment

byMnemonic :: String -> Op
byMnemonic "live" = Op "live" 1 [[direct]] 1 10 "alive"
byMnemonic "ld" = Op "ld" 2 [[direct, indirect], [register]] 2 5 "load"
byMnemonic "st" =  Op "st" 2 [[register], [indirect, register]] 3 5 "store"
byMnemonic "add" =  Op "add" 3 [[register], [register], [register]] 4 10 "addition"
byMnemonic "sub" =  Op "sub" 3 [[register], [register], [register]] 5 10 "substraction" 

byMnemonic "and" =  Op "and" 3 [[register, indirect, direct],
                                [register, indirect, direct],
                                [register]]  6 6 "bin and"

byMnemonic "or" =  Op "or" 3 [[register, indirect, direct],
                              [register, indirect, direct],
                              [register]]  7 6 "bin or"

byMnemonic "xor" =  Op "xor" 3 [[register, indirect, direct],
                                [register, indirect, direct],
                                [register]]  8 6 "bin xor"

byMnemonic "zjmp" =  Op "zjmp" 1 [[direct]]  9 20 "jump if zero"

byMnemonic "ldi" =  Op "ldi" 3 [[register, direct, indirect],
                                [direct, register],
                                [register]] 10 25 "load index"

byMnemonic "sti" =  Op "sti" 3 [[register],
                                [register, direct, indirect],
                                [direct, register]] 11 25 "store index"

byMnemonic "fork" =  Op "fork" 1 [[direct]]  12 800 "fork"
byMnemonic "lld" =  Op "lld" 2 [[direct], [indirect], [register]] 13 10 "long load"
byMnemonic "lldi" =  Op "lldi" 3 [[register, direct, indirect],
                                  [direct, register],
                                  [register]] 14 50 "long load idx"
byMnemonic "lfork" =  Op "lfork" 1 [[direct]]  15 1000 "long fork"
byMnemonic "aff" =  Op "aff" 1 [[register]]  16 2 "aff"

byMnemonic mnemonic = error $ "Unknown mnemonic found : " ++ mnemonic

opsNames = ["live", "ld", "st", "add", "sub", "and", "or", "xor", "zjmp", "ldi", "sti", "fork", "lld", "lldi", "lfork", "aff"]

opsbyCode :: [Op]
opsbyCode = [byMnemonic name | name <- opsNames]

byCode :: Int -> Op
byCode index = opsbyCode !! index

noOpCodeInstructions =
  [code (byMnemonic op) | op <- ["live", "zjmp", "fork", "lfork"]]

haveIndexInstructions =
  [code (byMnemonic op) | op <- ["zjmp", "ldi", "sti", "fork", "lldi", "lfork"]]

-- Defines for cycle informations ...
_cycleToDie = 1536
_cycleDelta = 5
_nbrLive = 40
