module Op (
  byMnemonic,
  byCode,
  getMnemonic,
  getNbrArgs,
  getArgsType,
  getCode,
  getNbrCycles,
  getComment
) where

-- import Data.Bits

-- Internals definitions
memSize = (8 * 1024 + 717)
idxMod =	512
maxArgsNumber	=	4
regNumber = 16

-- Asm syntax
commentChar = '#'
labelChar = ':'
directChar = '%'
separatorChar = ','
labelChars = "abcdefghijklmnopqrstuvwxyz_0123456789"
nameCmdString = ".name"
commentCmdString = ".comment"

-- Enum mask/flag for args types
register = 0x01
direct = 0x02
indirect = 0x04
label = 0x08

-- Size for memory access
indSize = 2
regSize = 4
dirSize = regSize

-- Param octet codage 

data Op = Op {
  mnemonic :: String,
  nbrArgs :: Int,
  argsType :: [[Int]],
  code :: Int,
  nbrCycles :: Int,
  comment :: String
} deriving (Show)

getMnemonic :: Op -> String
getMnemonic = mnemonic

getNbrArgs :: Op -> Int
getNbrArgs = nbrArgs

getArgsType :: Op -> [[Int]]
getArgsType = argsType

getCode :: Op -> Int
getCode = code

getNbrCycles :: Op -> Int
getNbrCycles = nbrCycles

getComment :: Op -> String
getComment = comment

byMnemonic "live" = Op "live"	1	[[direct]]	1	10	"alive"		
byMnemonic "ld" = Op "ld"	2	[[direct, indirect], [register]]	2	5	"load"		
byMnemonic "st" =  Op "st"	2	[[register], [indirect, register]]	3	5	"store"		
byMnemonic "add" =  Op "add"	3	[[register], [register], [register]]	4	10	"addition"	
byMnemonic "sub" =  Op "sub"	3	[[register], [register], [register]]	5	10	"substraction"	

byMnemonic "and" =  Op "and"	3	[[register, direct, indirect],
                         [register, direct, indirect],
                         [register]]		6	6	"bin and"	

byMnemonic "or" =  Op "or"	3	[[register, indirect, direct],
                       [register, indirect, direct],
                       [register]]		7	6	"bin or"	

byMnemonic "xor" =  Op "xor"	3	[[register, indirect, direct],
                         [register, indirect, direct],
                         [register]]		8	6	"bin xor"	

byMnemonic "zjmp" =  Op "zjmp"	1	[[direct]]		9	20	"jump if zero"	

byMnemonic "ldi" =  Op "ldi"	3	[[register, direct, indirect],
                         [direct, register],
                         [register]]	10	25	"load index"	
byMnemonic "sti" =  Op "sti"	3	[[register],
                         [register, direct, indirect],
                         [direct, register]]	11	25	"store index"	

byMnemonic "fork" =  Op "fork"	1	[[direct]]		12	800	"fork"		
byMnemonic "lld" =  Op "lld"	2	[[direct], [indirect], [register]]	13	10	"long load"	
byMnemonic "lldi" =  Op "lldi"	3	[[register, direct, indirect],
                           [direct, register],
                           [register]]	14	50	"long load idx"	
byMnemonic "lfork" =  Op "lfork"	1	[[direct]]		15	1000	"long fork"	
byMnemonic "aff" =  Op "aff"	1	[[register]]		16	2	"aff"

byMnemonic mnemonic = error $ "Unknown mnemonic found : " ++ mnemonic

opsNames = ["live", "ld", "st", "add", "sub", "and", "or", "xor", "zjmp", "ldi", "sti", "fork", "lld", "lldi", "lfork", "aff"]

opsbyCode = [byMnemonic name | name <- opsNames]

byCode index = opsbyCode !! index

-- Defines for cycle informations ...
cycleToDie = 1536
cycleDelta = 5
nbrLive = 40
