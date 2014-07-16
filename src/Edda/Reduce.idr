module Edda.Reduce

import Effects
import Effect.State

import Edda.Model

%access private

doReducePara : List (Block Raw) -> List (Block Raw)
doReducePara Nil       = Nil
doReducePara (x::y::z) = case doReduce x y of
                          Just yes => yes :: doReducePara z
                          Nothing  => x :: doReducePara (y::z)
  where
    doReduce : Block Raw -> Block Raw -> Maybe (Block Raw)
    doReduce (Para Model.Raw x) (Para Model.Raw y) = Just $ Para Raw (x ++ y)
    doReduce _            _            = Nothing
doReducePara x         = x

doReduce : List (Block Raw) -> {[STATE Nat]} Eff (List (Block Raw))
doReduce bs = do
    oldLen <- get
    let newBS = doReducePara bs
    if length newBS == oldLen
      then pure newBS
      else do
        put $ length newBS
        doReduce newBS

public
reduceParas : List (Block Raw) -> List (Block Raw)
reduceParas bs = runPure (do put $ length bs
                             doReduce bs)
