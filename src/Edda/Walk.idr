module Edda.Walk

import Edda.Model
import Edda.Model.Utils

{-
instance Walkable (Inline s) (Inline t) where
  walk f (Text s c)       = f $ Text s c
  walk f (Punc s pTy c)   = f $ Punc s pTy c
  walk f (Link s lTy u d) = f $ Link s lTy u (walk f d)
-}
