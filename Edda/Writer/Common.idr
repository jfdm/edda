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

-- -------------------------------------------------------------------- [ Body ]

strFromMaybe : (a -> String) -> Maybe a -> String
strFromMaybe f Nothing  = ""
strFromMaybe f (Just x) = f x

writeEddaFile : (Edda PRIME MODEL -> String)
              -> String
              -> Edda PRIME MODEL
              -> Eff (Either String ()) [FILE_IO ()]
writeEddaFile write fname doc = writeFile errFunc fname (write doc)
  where
    errFunc : String -> String
    errFunc fn = "Unable to create file handle " ++ show fn ++ " for writing."

-- --------------------------------------------------------------------- [ EOF ]
