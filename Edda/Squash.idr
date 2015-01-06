module Edda.Squash

import Effects
import Effect.State

import Edda.Model

%access private
%default total

-- ------------------------------------------------------- [ Generic Reduction ]
scanSquash2 : (a -> a -> Maybe a) -> List a -> List a
scanSquash2 _     Nil          = Nil
scanSquash2 squaFunc (x::xs) with (xs)
    | (y::ys) = case squaFunc x y of
                  Just yes => yes :: scanSquash2 squaFunc ys
                  Nothing  => x :: scanSquash2 squaFunc xs
    | Nil = x :: xs

covering
doSquash2 : (a -> a -> Maybe a) -> List a -> {[STATE Nat]} Eff (List a)
doSquash2 squaFunc bs = do
  oldLen <- get
  let newBS = scanSquash2 squaFunc bs
  if length newBS == oldLen
    then pure newBS
    else do
      put $ length newBS
      doSquash2 squaFunc newBS

covering
squash2 : (a -> a -> Maybe a) -> List a -> List a
squash2 squaFunc xs = runPureInit [length xs] (doSquash2 squaFunc xs)

-- --------------------------------------------------------- [ Block Reduction ]
squashRBPair : Block s -> Block s -> Maybe (Block s)
squashRBPair {s} (HRule s)  (HRule s) = Just $ HRule s
squashRBPair (Para xs) (Para ys)      = Just $ Para (xs ++ ys)
squashRBPair {s} (Empty s)  (Empty s) = Just $ Empty s
squashRBPair _              _         = Nothing

public
covering
squash2Blocks : List (Block s) -> List (Block s)
squash2Blocks = squash2 (squashRBPair)


-- -------------------------------------------------------- [ Inline Reduction ]
squashIPair : Inline s -> Inline s -> Maybe (Inline s)
squashIPair Space  Space  = Just Space
squashIPair Hyphen Hyphen = Just EnDash
squashIPair _      _      = Nothing

public
covering
squash2Inlines : List (Inline s) -> List (Inline s)
squash2Inlines = squash2 (squashIPair)

-- --------------------------------------------------- [ Triple Punc Squashing ]
public
covering
squash3 : (a -> a -> a -> Maybe a) -> List a -> List a
squash3 _        Nil     = Nil
squash3 squaFunc (x::xs) with (xs)
  | (y::z::zs) = case squaFunc x y z of
                  Just yes => yes :: squash3 squaFunc zs
                  Nothing  => x :: squash3 squaFunc xs
  | (y::ys)    = x :: xs
  | Nil        = x :: xs

squashITriples : Inline s -> Inline s -> Inline s -> Maybe (Inline s)
squashITriples Period Period Period = Just Ellipsis
squashITriples Hyphen Hyphen Hyphen = Just EmDash
squashITriples _      _      _      = Nothing

public
covering
squash3Inlines : List (Inline s) -> List (Inline s)
squash3Inlines = squash3 (squashITriples)
