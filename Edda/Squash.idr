module Edda.Squash

import Effects
import Effect.State

import Edda.Model

%access private
%default total

-- ----------------------------------------------------------- [ Double Squash ]
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

public
covering
squash2By : (a -> a -> Maybe a) -> List a -> List a
squash2By squaFunc xs = runPureInit [length xs] (doSquash2 squaFunc xs)

squashEddaPair : Edda s ty -> Edda s ty -> Maybe (Edda s ty)
squashEddaPair {s} (HRule s)  (HRule s) = Just $ HRule s
squashEddaPair (Para xs) (Para ys)      = Just $ Para (xs ++ ys)
squashEddaPair {s} (Empty s)  (Empty s) = Just $ Empty s
squashEddaPair Space          Space     = Just Space
squashEddaPair Hyphen         Hyphen    = Just EnDash
squashEddaPair _              _         = Nothing

public
covering
squash2 : List (Edda s ty) -> List (Edda s ty)
squash2 = squash2By (squashEddaPair)


-- --------------------------------------------------- [ Triple Punc Squashing ]
public
covering
squash3By : (a -> a -> a -> Maybe a) -> List a -> List a
squash3By _        Nil     = Nil
squash3By squaFunc (x::xs) with (xs)
  | (y::z::zs) = case squaFunc x y z of
                  Just yes => yes :: squash3By squaFunc zs
                  Nothing  => x :: squash3By squaFunc xs
  | (y::ys)    = x :: xs
  | Nil        = x :: xs

squashEddaTriples : Edda s ty -> Edda s ty -> Edda s ty -> Maybe (Edda s ty)
squashEddaTriples Period Period Period = Just Ellipsis
squashEddaTriples Hyphen Hyphen Hyphen = Just EmDash
squashEddaTriples _      _      _      = Nothing

public
covering
squash3 : List (Edda s ty) -> List (Edda s ty)
squash3 = squash3By (squashEddaTriples)
