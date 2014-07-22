module Edda.Walk

import Edda.Model
import Edda.Model.Utils

--%default total

-- TODO walk attributes
-- TODO walk maybe

class Walkable a z where
  walk : (a -> a) -> z -> z
--  search : (a -> res) -> z -> res

instance Walkable a b => Walkable a (List b) where
  walk f xs = map (walk f) xs

instance (Walkable a b, Walkable a c) => Walkable a (b,c) where
  walk f (x,y) = (walk f x, walk f y)

instance Walkable (Inline s) (Inline s) where
  walk f (Punc c)       = f $ Punc c
  walk f (Raw ty  t)    = f $ Raw ty t

  walk f (Font ty t)     = f $ Font ty t
  walk f (Mark ty xs)    = f $ Mark ty (walk f xs)
  walk f (Link ty u xs)  = f $ Link ty u xs

  walk f (Text t) = f $ (Text t)
  walk f (Sans t) = f $ (Sans t)
  walk f (Scap t) = f $ (Scap t)
  walk f (Mono t) = f $ (Mono t)

  walk f (Verb v) = f $ (Verb v)
  walk f (Code v) = f $ (Code v)
  walk f (Math v) = f $ (Math v)

  walk f (Emph xs)   = f $ Emph (walk f xs)
  walk f (Bold xs)   = f $ Bold (walk f xs)
  walk f (Strike xs) = f $ Strike (walk f xs)
  walk f (Uline xs)  = f $ Uline (walk f xs)

  walk f (Quote ty xs)  = f $ Quote ty (walk f xs)
  walk f (Parens ty xs) = f $ Parens ty (walk f xs)

  walk f (Ref l)      = f $ Ref l
  walk f (Cite ty l)  = f $ Cite ty l
  walk f (Hyper l xs) = f $ Hyper l (walk f xs)
  walk f (MiscPunc c) = f $ MiscPunc c

  walk f Space      = f $ Space
  walk f Newline    = f $ Newline
  walk f Tab        = f $ Tab
  walk f LBrace     = f $ LBrace
  walk f RBrace     = f $ RBrace
  walk f LParen     = f $ LParen
  walk f RParen     = f $ RParen
  walk f LBrack     = f $ LBrack
  walk f RBrack     = f $ RBrack
  walk f LAngle     = f $ LAngle
  walk f RAngle     = f $ RAngle
  walk f Dollar     = f $ Dollar
  walk f Colon      = f $ Colon
  walk f Semi       = f $ Semi
  walk f EnDash     = f $ EnDash
  walk f EmDash     = f $ EmDash
  walk f FSlash     = f $ FSlash
  walk f BSlash     = f $ BSlash
  walk f Apostrophe = f $ Apostrophe
  walk f SMark      = f $ SMark
  walk f Comma      = f $ Comma
  walk f Plus       = f $ Plus
  walk f Minus      = f $ Minus
  walk f Ellipsis   = f $ Ellipsis
  walk f Hyphen     = f $ Hyphen
  walk f Bang       = f $ Bang
  walk f Period     = f $ Period
  walk f QMark      = f $ QMark
  walk f Hash       = f $ Hash
  walk f Equals     = f $ Equals
  walk f Pipe       = f $ Pipe

instance Walkable (Block s) (Inline s) where
  walk f (Punc c)       = Punc c
  walk f (Raw ty  t)    = Raw ty t

  walk f (Font ty t)     = Font ty t
  walk f (Mark ty xs)    = Mark ty (walk f xs)
  walk f (Link ty u xs)  = Link ty u xs

  walk f (Text t) = (Text t)
  walk f (Sans t) = (Sans t)
  walk f (Scap t) = (Scap t)
  walk f (Mono t) = (Mono t)

  walk f (Verb v) = (Verb v)
  walk f (Code v) = (Code v)
  walk f (Math v) = (Math v)

  walk f (Emph xs)   = Emph (walk f xs)
  walk f (Bold xs)   = Bold (walk f xs)
  walk f (Strike xs) = Strike (walk f xs)
  walk f (Uline xs)  = Uline (walk f xs)

  walk f (Quote ty xs)  = Quote ty (walk f xs)
  walk f (Parens ty xs) = Parens ty (walk f xs)

  walk f (Ref l)      = Ref l
  walk f (Cite ty l)  = Cite ty l
  walk f (Hyper l xs) = Hyper l (walk f xs)
  walk f (MiscPunc c) = MiscPunc c

  walk f Space      = Space
  walk f Newline    = Newline
  walk f Tab        = Tab
  walk f LBrace     = LBrace
  walk f RBrace     = RBrace
  walk f LParen     = LParen
  walk f RParen     = RParen
  walk f LBrack     = LBrack
  walk f RBrack     = RBrack
  walk f LAngle     = LAngle
  walk f RAngle     = RAngle
  walk f Dollar     = Dollar
  walk f Colon      = Colon
  walk f Semi       = Semi
  walk f EnDash     = EnDash
  walk f EmDash     = EmDash
  walk f FSlash     = FSlash
  walk f BSlash     = BSlash
  walk f Apostrophe = Apostrophe
  walk f SMark      = SMark
  walk f Comma      = Comma
  walk f Plus       = Plus
  walk f Minus      = Minus
  walk f Ellipsis   = Ellipsis
  walk f Hyphen     = Hyphen
  walk f Bang       = Bang
  walk f Period     = Period
  walk f QMark      = QMark
  walk f Hash       = Hash
  walk f Equals     = Equals
  walk f Pipe       = Pipe

instance Walkable (Inline s) (Block s) where
  walk {s} f (Para s xs) = Para s $ walk f xs

  walk {s} f (Header s d l t)        = Header s d l (walk f t)
  walk {s} f (Figure s l c as fig)   = Figure s l (walk f c) as (walk f fig)
  walk {s} f (TextBlock s l c as xs) = TextBlock s l c as (walk f xs)
  walk {s} f (VerbBlock s l c as v)  = VerbBlock s l c as v
  walk {s} f (Table s l c tbl)       = Table s l c tbl

  walk f (OList xs)  = OList (walk f xs)
  walk f (BList xs)  = BList (walk f xs)
  walk f (DList kvs) = DList (walk f kvs)

  walk f (Listing l c as s)  = Listing l c as s
  walk f (Equation l s)      = Equation l s
  walk f (Quotation l xs)    = Quotation l (walk f xs)
  walk f (Theorem l c ty xs) = Theorem l c ty (walk f xs)

  walk {s} f (Empty s)   = Empty s

instance Walkable (Block s) (Block s) where
  walk {s} f (Para s xs) = f $ Para s $ walk f xs

  walk {s} f (Header s d l t)        = f $ Header s d l (walk f t)
  walk {s} f (Figure s l c as fig)   = f $ Figure s l (walk f c) as (walk f fig)
  walk {s} f (TextBlock s l c as xs) = f $ TextBlock s l c as (walk f xs)
  walk {s} f (VerbBlock s l c as v)  = f $ VerbBlock s l c as v
  walk {s} f (Table s l c tbl)       = f $ Table s l c tbl

  walk f (OList xs)  = f $ OList (walk f xs)
  walk f (BList xs)  = f $ BList (walk f xs)
  walk f (DList kvs) = f $ DList (walk f kvs)

  walk f (Listing l c as s)  = f $ Listing l c as s
  walk f (Equation l s)      = f $ Equation l s
  walk f (Quotation l xs)      = f $ Quotation l (walk f xs)
  walk f (Theorem l c ty xs) = f $ Theorem l c ty (walk f xs)

  walk {s} f (Empty s)   = Empty s

instance Walkable (Block s) (Edda s) where
  walk {s} f (MkEdda s as xs) = MkEdda s as (walk f xs)
  walk f (MkEddaDoc ps xs)    = MkEddaDoc ps (walk f xs)
  walk f (MkEddaSimple ps xs) = MkEddaSimple ps (walk f xs)

instance Walkable (Inline s) (Edda s) where
  walk {s} f (MkEdda s as xs) = MkEdda s as (walk f xs)
  walk f (MkEddaDoc ps xs)    = MkEddaDoc ps (walk f xs)
  walk f (MkEddaSimple ps xs) = MkEddaSimple ps (walk f xs)

instance Walkable (Edda s) (Edda s) where
  walk {s} f (MkEdda s as xs) = f $ MkEdda s as xs
  walk f (MkEddaDoc ps xs)    = f $ MkEddaDoc ps xs
  walk f (MkEddaSimple ps xs) = f $ MkEddaSimple ps xs
