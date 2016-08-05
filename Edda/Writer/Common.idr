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

%access export
-- -------------------------------------------------------------------- [ Body ]

strFromMaybe : (a -> String) -> Maybe a -> String
strFromMaybe f Nothing  = ""
strFromMaybe f (Just x) = f x

writeEddaFile : (Edda PRIME MODEL -> String)
              -> String
              -> Edda PRIME MODEL
              -> Eff (FileOpSuccess) [FILE ()]
writeEddaFile write fname doc = writeFile fname (write doc)

-- --------------------------------------------------------------------- [ EOF ]
