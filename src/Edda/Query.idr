module Edda.Query

import Effects
import Effect.State
import Effect.StdIO

import Edda.Model

class Queryable a b where
  query : Monoid res => (a -> res) -> b -> res

instance Queryable a b => Queryable a (List b) where
  query f xs = foldr (<+>) neutral $ map (query f) xs

instance (Queryable a b, Queryable a c) => Queryable a (b,c) where
  query f (x,y) = (query f x) <+> (query f y)

instance Queryable (Inline Prime) (Inline Prime) where
  query f (Text t) = f $ (Text t)
  query f (Sans t) = f $ (Sans t)
  query f (Scap t) = f $ (Scap t)
  query f (Mono t) = f $ (Mono t)

  query f (Verb v) = f $ (Verb v)
  query f (Code v) = f $ (Code v)
  query f (Math v) = f $ (Math v)

  query f (Emph xs)   = f (Emph xs) <+> (query f xs)
  query f (Bold xs)   = f (Bold xs) <+> (query f xs)
  query f (Strike xs) = f (Strike xs) <+> (query f xs)
  query f (Uline xs)  = f (Uline xs) <+> (query f xs)

  query f (Quote ty xs)  = f (Quote ty xs) <+> (query f xs)
  query f (Parens ty xs) = f (Parens ty xs) <+> (query f xs)

  query f (Ref l)       = f $ Ref l
  query f (Cite ty l)   = f $ Cite ty l
  query f (Hyper l xs)  = f (Hyper l xs) -- Maybe <+> (query f xs)
  query f (FNote l xs)  = f (FNote l xs) -- Maybe <+> (query f xs)
  query f (MiscPunc c)  = f $ MiscPunc c

  query f Space      = f $ Space
  query f Newline    = f $ Newline
  query f Tab        = f $ Tab
  query f LBrace     = f $ LBrace
  query f RBrace     = f $ RBrace
  query f LParen     = f $ LParen
  query f RParen     = f $ RParen
  query f LBrack     = f $ LBrack
  query f RBrack     = f $ RBrack
  query f LAngle     = f $ LAngle
  query f RAngle     = f $ RAngle
  query f Dollar     = f $ Dollar
  query f Colon      = f $ Colon
  query f Semi       = f $ Semi
  query f EnDash     = f $ EnDash
  query f EmDash     = f $ EmDash
  query f FSlash     = f $ FSlash
  query f BSlash     = f $ BSlash
  query f Apostrophe = f $ Apostrophe
  query f SMark      = f $ SMark
  query f Comma      = f $ Comma
  query f Plus       = f $ Plus
  query f Ellipsis   = f $ Ellipsis
  query f Hyphen     = f $ Hyphen
  query f Bang       = f $ Bang
  query f Period     = f $ Period
  query f QMark      = f $ QMark
  query f Hash       = f $ Hash
  query f Equals     = f $ Equals
  query f Pipe       = f $ Pipe

-- @TODO Captions
instance Queryable (Inline Prime) (Block Prime) where
  query f (HRule Prime)             = neutral
  query f (Empty Prime)             = neutral
  query f (Header Prime d l t)      = query f t
  query f (Figure Prime l c as fig) = query f c <+> query f fig
  query f (Table Prime l c tbl)     = neutral    -- caption and table
  query f (DList Prime kvs)         = query f kvs

  query f (OList xs)  = query f xs
  query f (BList xs)  = query f xs

  query f (Comment ss)          = neutral
  query f (Equation l s)        = neutral
  query f (Literal l c ss)      = neutral -- caption
  query f (Listing l c ty ops as s) = neutral -- caption

  query f (Para xs)        = query f xs
  query f (Quotation l xs) = query f xs

  query f (Theorem l c xs)     = query f xs -- caption
  query f (Corollary l c xs)   = query f xs -- caption
  query f (Lemma l c xs)       = query f xs -- caption
  query f (Proposition l c xs) = query f xs -- caption
  query f (Proof l c xs)       = query f xs -- caption
  query f (Definition l c xs)  = query f xs -- caption
  query f (Exercise l c xs)    = query f xs -- caption
  query f (Note l c xs)        = query f xs -- caption
  query f (Remark l c xs)      = query f xs -- caption
  query f (Problem l c xs)     = query f xs -- caption
  query f (Question l c xs)    = query f xs -- caption
  query f (Solution l c xs)    = query f xs -- caption
  query f (Example l c xs)     = query f xs -- caption

instance Queryable (Block Prime) (Inline Prime) where
  query f (Text t) = neutral
  query f (Sans t) = neutral
  query f (Scap t) = neutral
  query f (Mono t) = neutral

  query f (Verb v) = neutral
  query f (Code v) = neutral
  query f (Math v) = neutral

  query f (Emph xs)   = query f xs
  query f (Bold xs)   = query f xs
  query f (Strike xs) = query f xs
  query f (Uline xs)  = query f xs

  query f (Quote ty xs)  = query f xs
  query f (Parens ty xs) = query f xs

  query f (Ref l)      = neutral
  query f (Cite ty l)  = neutral
  query f (Hyper l xs) = neutral -- maybes query f xs
  query f (FNote l xs) = neutral -- maybes qeuery f xs
  query f (MiscPunc c) = neutral

  query f Space      = neutral
  query f Newline    = neutral
  query f Tab        = neutral
  query f LBrace     = neutral
  query f RBrace     = neutral
  query f LParen     = neutral
  query f RParen     = neutral
  query f LBrack     = neutral
  query f RBrack     = neutral
  query f LAngle     = neutral
  query f RAngle     = neutral
  query f Dollar     = neutral
  query f Colon      = neutral
  query f Semi       = neutral
  query f EnDash     = neutral
  query f EmDash     = neutral
  query f FSlash     = neutral
  query f BSlash     = neutral
  query f Apostrophe = neutral
  query f SMark      = neutral
  query f Comma      = neutral
  query f Plus       = neutral
  query f Minus      = neutral
  query f Ellipsis   = neutral
  query f Hyphen     = neutral
  query f Bang       = neutral
  query f Period     = neutral
  query f QMark      = neutral
  query f Hash       = neutral
  query f Equals     = neutral
  query f Pipe       = neutral

-- @TODO Query Table
instance Queryable (Block Prime) (Block Prime) where
  query f (HRule Prime)               = f (HRule Prime)
  query f (Empty Prime)               = f (Empty Prime)
  query f (Header Prime d l t)        = f (Header Prime d l t) <+> (query f t)
  query f (Figure Prime l c as fig)   = f (Figure Prime l c as fig) <+> (query f fig) -- caption
  query f (Table Prime l c tbl)       = f (Table Prime l c tbl) -- caption
  query f (DList Prime kvs)           = f (DList Prime kvs) <+> (query f kvs)

  query f (OList xs)  = f (OList xs) <+> (query f xs)
  query f (BList xs)  = f (BList xs) <+> (query f xs)

  query f (Comment ss)          = f (Comment ss)
  query f (Equation l s)        = f (Equation l s)
  query f (Literal l c ss)      = f (Literal l c ss)      -- caption
  query f (Listing l c ty ops as s) = f (Listing l c ty ops as s) -- caption

  query f (Para xs)        = f (Para xs) <+> query f xs
  query f (Quotation l xs) = f (Quotation l xs) <+> (query f xs)

  query f (Theorem l c xs)     = f (Theorem l c xs) <+> (query f xs) -- caption
  query f (Corollary l c xs)   = f (Corollary l c xs) <+> (query f xs) -- caption
  query f (Lemma l c xs)       = f (Lemma l c xs) <+> (query f xs) -- caption
  query f (Proposition l c xs) = f (Proposition l c xs) <+> (query f xs) -- caption
  query f (Proof l c xs)       = f (Proof l c xs) <+> (query f xs) -- caption
  query f (Definition l c xs)  = f (Definition l c xs) <+> (query f xs) -- caption
  query f (Exercise l c xs)    = f (Exercise l c xs) <+> (query f xs) -- caption
  query f (Note l c xs)        = f (Note l c xs) <+> (query f xs) -- caption
  query f (Remark l c xs)      = f (Remark l c xs) <+> (query f xs) -- caption
  query f (Problem l c xs)     = f (Problem l c xs) <+> (query f xs) -- caption
  query f (Question l c xs)    = f (Question l c xs) <+> (query f xs) -- caption
  query f (Solution l c xs)    = f (Solution l c xs) <+> (query f xs) -- caption
  query f (Example l c xs)     = f (Example l c xs) <+> (query f xs) -- caption

instance Queryable (Block Prime) (Edda Prime) where
  query f (MkEdda Prime as xs) = query f xs

instance Queryable (Inline Prime) (Edda Prime) where
  query f (MkEdda Prime as xs) = query f xs

instance Queryable (Edda Prime) (Edda Prime) where
  query f (MkEdda Prime as xs) = f $ MkEdda Prime as xs
