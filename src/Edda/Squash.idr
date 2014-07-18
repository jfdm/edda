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

squashRBPair : Block s -> Block s -> Maybe (Block s)
squashRBPair {s} (Para s x) (Para s y) = Just $ Para s (x ++ y)
squashRBPair _                  _      = Nothing


squashRawBlocks : List (Block s) -> List (Block s)
squashRawBlocks bs = squash (squashRBPair) bs


-- -------------------------------------------------------- [ Inline Reduction ]
squashRIPair : Inline s -> Inline s -> Maybe (Inline s)
squashRIPair Space Space = Just Space
squashRIPair _            _        = Nothing


squashRawInlines : List (Inline s) -> List (Inline s)
squashRawInlines is = squash (squashRIPair) is



{-

squashRIPair (Punc x) (Punc y) = case x /= y of
    True  => Nothing
    False => case x of
               ' ' => Just $ Punc x
               '\n' => Just $ Punc x

-}
