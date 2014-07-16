module Edda.Squash

import Effects
import Effect.State

import Edda.Model

%access public

-- ------------------------------------------------------- [ Generic Reduction ]
scanSquash : (a -> a -> Maybe a) -> List a -> List a
scanSquash _     Nil       = Nil
scanSquash squaFunc (x::y::z) = case squaFunc x y of
                               Just yes => yes :: scanSquash squaFunc z
                               Nothing  => x :: scanSquash squaFunc (y::z)
scanSquash _     x         = x

doSquash : (a -> a -> Maybe a) -> List a -> {[STATE Nat]} Eff (List a)
doSquash squaFunc bs = do
  oldLen <- get
  let newBS = scanSquash squaFunc bs
  if length newBS == oldLen
    then pure newBS
    else do
      put $ length newBS
      doSquash squaFunc newBS


squash : (a -> a -> Maybe a) -> List a -> List a
squash squaFunc xs = runPureInit [length xs] (doSquash squaFunc xs)

-- --------------------------------------------------------- [ Block Reduction ]

squashRBPair : Block Raw -> Block Raw -> Maybe (Block Raw)
squashRBPair (Para Model.Raw x) (Para Model.Raw y) = Just $ Para Raw (x ++ y)
squashRBPair _                  _                  = Nothing


squashRawBlocks : List (Block Raw) -> List (Block Raw)
squashRawBlocks bs = squash (squashRBPair) bs


-- -------------------------------------------------------- [ Inline Reduction ]
squashRIPair : Inline Raw -> Inline Raw -> Maybe (Inline Raw)
squashRIPair (Text Model.Raw x) (Text Model.Raw y)= Just $ Text Raw (x ++ " " ++ y)
squashRIPair (Punc Model.Raw Space x) (Punc Model.Raw Space y) = Just $ Punc Raw Space x
squashRIPair _  (Punc Model.Raw Newline x) = Just $ Punc Raw Space ' '
squashRIPair _                  _                          = Nothing


squashRawInlines : List (Inline Raw) -> List (Inline Raw)
squashRawInlines is = squash (squashRIPair) is
