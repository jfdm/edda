-- -------------------------------------------------------------- [ Common.idr ]
-- Module    : Common.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Edda.Writer.Common

import Effects
import Effect.Exception
import Effect.File
import Effect.StdIO

import Edda.Model

strFromMaybe : (a -> String) -> Maybe a -> String
strFromMaybe f Nothing  = ""
strFromMaybe f (Just x) = f x

writeEddaFile : (Edda PRIME MODEL -> String)
              -> String
              -> Edda PRIME MODEL
              -> Eff () [FILE_IO (), EXCEPTION String]
writeEddaFile write fname doc =
  case !(open fname Write) of
    True => do
      writeString $ write doc
      close
    False => raise "Unable to create file handle for writing."

-- --------------------------------------------------------------------- [ EOF ]
