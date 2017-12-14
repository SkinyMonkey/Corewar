module Op (
  byMnemonic,
  byCode,
  getMnemonic,
  getNbrArgs,
  getArgsTypes,
  getCode,
  getNbrCycles,
  getComment,
  noOpCodeInstructions,
  ArgType(..),
-- FIXME : debug
  opsNames,
  Op,
) where

-- import Data.Bits
import Data.Word

-- Internals definitions
_memSize = (8 * 1024 + 717)
_idxMod = 512
_maxArgsNumber = 4
_regNumber = 16

-- Asm syntax
_commentChar = '#'
_labelChar = ':'
_DirectChar = '%'
_separatorChar = ','
_labelChars = "abcdefghijklmnopqrstuvwxyz_0123456789"
_nameCmdString = ".name"
_commentCmdString = ".comment"

data ArgType = Register | Direct | Indirect | Label deriving (Show, Eq)

-- Size for memory access
_indSize = 2
_regSize = 4
_dirSize = _regSize

-- Param octet codage 

data Op = Op {
  mnemonic :: String,
  nbrArgs :: Int,
  argsType :: [[ArgType]],
  code :: Word8,
  nbrCycles :: Int,
  comment :: String
} deriving (Show)

getMnemonic :: Op -> String
getMnemonic = mnemonic

getNbrArgs :: Op -> Int
getNbrArgs = nbrArgs

getArgsTypes :: Op -> [[ArgType]]
getArgsTypes = argsType

getCode :: Op -> Word8
getCode = code

getNbrCycles :: Op -> Int
getNbrCycles = nbrCycles

getComment :: Op -> String
getComment = comment

byMnemonic :: String -> Op
byMnemonic "live" = Op "live" 1 [[Direct]] 1 10 "alive"  
byMnemonic "ld" = Op "ld" 2 [[Direct, Indirect], [Register]] 2 5 "load"  
byMnemonic "st" =  Op "st" 2 [[Register], [Indirect, Register]] 3 5 "store"  
byMnemonic "add" =  Op "add" 3 [[Register], [Register], [Register]] 4 10 "addition" 
byMnemonic "sub" =  Op "sub" 3 [[Register], [Register], [Register]] 5 10 "substraction" 

byMnemonic "and" =  Op "and" 3 [[Register, Direct, Indirect],
                         [Register, Direct, Indirect],
                         [Register]]  6 6 "bin and" 

byMnemonic "or" =  Op "or" 3 [[Register, Indirect, Direct],
                       [Register, Indirect, Direct],
                       [Register]]  7 6 "bin or" 

byMnemonic "xor" =  Op "xor" 3 [[Register, Indirect, Direct],
                         [Register, Indirect, Direct],
                         [Register]]  8 6 "bin xor" 

byMnemonic "zjmp" =  Op "zjmp" 1 [[Direct]]  9 20 "jump if zero" 

byMnemonic "ldi" =  Op "ldi" 3 [[Register, Direct, Indirect],
                         [Direct, Register],
                         [Register]] 10 25 "load index" 
byMnemonic "sti" =  Op "sti" 3 [[Register],
                         [Register, Direct, Indirect],
                         [Direct, Register]] 11 25 "store index" 

byMnemonic "fork" =  Op "fork" 1 [[Direct]]  12 800 "fork"  
byMnemonic "lld" =  Op "lld" 2 [[Direct], [Indirect], [Register]] 13 10 "long load" 
byMnemonic "lldi" =  Op "lldi" 3 [[Register, Direct, Indirect],
                           [Direct, Register],
                           [Register]] 14 50 "long load idx" 
byMnemonic "lfork" =  Op "lfork" 1 [[Direct]]  15 1000 "long fork" 
byMnemonic "aff" =  Op "aff" 1 [[Register]]  16 2 "aff"

byMnemonic mnemonic = error $ "Unknown mnemonic found : " ++ mnemonic

opsNames = ["live", "ld", "st", "add", "sub", "and", "or", "xor", "zjmp", "ldi", "sti", "fork", "lld", "lldi", "lfork", "aff"]

opsbyCode :: [Op]
opsbyCode = [byMnemonic name | name <- opsNames]

byCode :: Int -> Op
byCode index = opsbyCode !! index

noOpCodeInstructions =
  [code (byMnemonic op) | op <- ["live", "zjmp", "fork", "lfork"]]

-- Defines for cycle informations ...
_cycleToDie = 1536
_cycleDelta = 5
_nbrLive = 40
