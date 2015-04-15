module ParseAsm (
  parseMetadata,
  parseLabel
) where

import ParseBase

parseMetadata field cstring = (parseId $ tail field) && (parseString cstring)

parseLabel candidate =  parseId $ take (length(candidate) - 1) candidate
