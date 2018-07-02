module Text.Markup.Edda.Query

import Data.AVL.Dict

import Text.Markup.Edda.Model.Common
import Text.Markup.Edda.Model.Raw
import Text.Markup.Edda.Model.Processed

%access export

interface Queryable a b where
  query : Monoid res => (a -> res) -> b -> res

Queryable a b => Queryable a (List b) where
  query f xs = foldr (<+>) neutral $ map (query f) xs

(Queryable a b, Queryable a c) => Queryable a (b,c) where
  query f (x,y) = (query f x) <+> (query f y)

Queryable (Edda tyA) (Edda tyB) where

  -- INLINE -> INLINE
  query {tyA=INLINE} {tyB=INLINE} f (Emph xs)   = f (Emph xs) <+> (query f xs)
  query {tyA=INLINE} {tyB=INLINE} f (Bold xs)   = f (Bold xs) <+> (query f xs)
  query {tyA=INLINE} {tyB=INLINE} f (Strike xs) = f (Strike xs) <+> (query f xs)
  query {tyA=INLINE} {tyB=INLINE} f (Uline xs)  = f (Uline xs) <+> (query f xs)

  query {tyA=INLINE} {tyB=INLINE} f (Quote ty xs)  = f (Quote ty xs) <+> (query f xs)
  query {tyA=INLINE} {tyB=INLINE} f (Parens ty xs) = f (Parens ty xs) <+> (query f xs)

  query {tyA=INLINE} {tyB=INLINE} f (Hyper l xs)  = f (Hyper l xs) <+> (query f xs)
  query {tyA=INLINE} {tyB=INLINE} f (FNote l xs)  = f (FNote l xs) <+> (query f xs)
  query {tyA=INLINE} {tyB=INLINE} f inline  = f inline

  -- INLINE to BLOCK
  query {tyA=INLINE} {tyB=BLOCK} f (Section d l t as bs) = query f t <+> query f bs
  query {tyA=INLINE} {tyB=BLOCK} f (Figure l c as fig) = query f c <+> query f fig
  query {tyA=INLINE} {tyB=BLOCK} f (DList kvs)         = query f kvs

  query {tyA=INLINE} {tyB=BLOCK} f (OList xs)  = query f xs
  query {tyA=INLINE} {tyB=BLOCK} f (BList xs)  = query f xs

  query {tyA=INLINE} {tyB=BLOCK} f (Literal l c ss)      = neutral <+> query f c
  query {tyA=INLINE} {tyB=BLOCK} f (Listing l c ty ops as s) = neutral <+> query f c

  query {tyA=INLINE} {tyB=BLOCK} f (Para xs)        = query f xs
  query {tyA=INLINE} {tyB=BLOCK} f (Quotation l xs) = query f xs

  query {tyA=INLINE} {tyB=BLOCK} f (Theorem l c xs)     = query f xs <+> query f c
  query {tyA=INLINE} {tyB=BLOCK} f (Corollary l c xs)   = query f xs <+> query f c
  query {tyA=INLINE} {tyB=BLOCK} f (Lemma l c xs)       = query f xs <+> query f c
  query {tyA=INLINE} {tyB=BLOCK} f (Proposition l c xs) = query f xs <+> query f c
  query {tyA=INLINE} {tyB=BLOCK} f (Proof l c xs)       = query f xs <+> query f c
  query {tyA=INLINE} {tyB=BLOCK} f (Definition l c xs)  = query f xs <+> query f c
  query {tyA=INLINE} {tyB=BLOCK} f (Exercise l c xs)    = query f xs <+> query f c
  query {tyA=INLINE} {tyB=BLOCK} f (Note l c xs)        = query f xs <+> query f c
  query {tyA=INLINE} {tyB=BLOCK} f (Remark l c xs)      = query f xs <+> query f c
  query {tyA=INLINE} {tyB=BLOCK} f (Problem l c xs)     = query f xs <+> query f c
  query {tyA=INLINE} {tyB=BLOCK} f (Question l c xs)    = query f xs <+> query f c
  query {tyA=INLINE} {tyB=BLOCK} f (Solution l c xs)    = query f xs <+> query f c
  query {tyA=INLINE} {tyB=BLOCK} f (Example l c xs)     = query f xs <+> query f c
  query {tyA=INLINE} {tyB=BLOCK} f block = neutral

  -- BLOCK to INLINE

  query {tyA=BLOCK} {tyB=INLINE} f (Emph xs)   = query f xs
  query {tyA=BLOCK} {tyB=INLINE} f (Bold xs)   = query f xs
  query {tyA=BLOCK} {tyB=INLINE} f (Strike xs) = query f xs
  query {tyA=BLOCK} {tyB=INLINE} f (Uline xs)  = query f xs

  query {tyA=BLOCK} {tyB=INLINE} f (Quote ty xs)  = query f xs
  query {tyA=BLOCK} {tyB=INLINE} f (Parens ty xs) = query f xs

  query {tyA=BLOCK} {tyB=INLINE} f (Ref l)      = neutral
  query {tyA=BLOCK} {tyB=INLINE} f (Cite ty l)  = neutral
  query {tyA=BLOCK} {tyB=INLINE} f (Hyper l xs) = neutral <+> query f xs
  query {tyA=BLOCK} {tyB=INLINE} f (FNote l xs) = neutral <+> query f xs
  query {tyA=BLOCK} {tyB=INLINE} f (MiscPunc c) = neutral

  query {tyA=BLOCK} {tyB=INLINE} f inline = neutral

  -- BLOCK to BLOCK
  query {tyA=BLOCK} {tyB=BLOCK} f (Section d l t as bs) = f (Section d l t as bs) <+> (query f t) <+> query f bs
  query {tyA=BLOCK} {tyB=BLOCK} f (Figure l c as fig)   = f (Figure l c as fig) <+> (query f fig) <+> query f c
  query {tyA=BLOCK} {tyB=BLOCK} f (DList kvs)           = f (DList kvs) <+> (query f kvs)

  query {tyA=BLOCK} {tyB=BLOCK} f (OList xs)  = f (OList xs) <+> (query f xs)
  query {tyA=BLOCK} {tyB=BLOCK} f (BList xs)  = f (BList xs) <+> (query f xs)

  query {tyA=BLOCK} {tyB=BLOCK} f (Literal l c ss)      = f (Literal l c ss) <+> query f c
  query {tyA=BLOCK} {tyB=BLOCK} f (Listing l c ty ops as s) = f (Listing l c ty ops as s) <+> query f c

  query {tyA=BLOCK} {tyB=BLOCK} f (Para xs)        = f (Para xs) <+> query f xs
  query {tyA=BLOCK} {tyB=BLOCK} f (Quotation l xs) = f (Quotation l xs) <+> (query f xs)

  query {tyA=BLOCK} {tyB=BLOCK} f (Theorem l c xs)     = f (Theorem l c xs) <+> (query f xs) <+> query f c
  query {tyA=BLOCK} {tyB=BLOCK} f (Corollary l c xs)   = f (Corollary l c xs) <+> (query f xs) <+> query f c
  query {tyA=BLOCK} {tyB=BLOCK} f (Lemma l c xs)       = f (Lemma l c xs) <+> (query f xs) <+> query f c
  query {tyA=BLOCK} {tyB=BLOCK} f (Proposition l c xs) = f (Proposition l c xs) <+> (query f xs) <+> query f c
  query {tyA=BLOCK} {tyB=BLOCK} f (Proof l c xs)       = f (Proof l c xs) <+> (query f xs) <+> query f c
  query {tyA=BLOCK} {tyB=BLOCK} f (Definition l c xs)  = f (Definition l c xs) <+> (query f xs) <+> query f c
  query {tyA=BLOCK} {tyB=BLOCK} f (Exercise l c xs)    = f (Exercise l c xs) <+> (query f xs) <+> query f c
  query {tyA=BLOCK} {tyB=BLOCK} f (Note l c xs)        = f (Note l c xs) <+> (query f xs) <+> query f c
  query {tyA=BLOCK} {tyB=BLOCK} f (Remark l c xs)      = f (Remark l c xs) <+> (query f xs) <+> query f c
  query {tyA=BLOCK} {tyB=BLOCK} f (Problem l c xs)     = f (Problem l c xs) <+> (query f xs) <+> query f c
  query {tyA=BLOCK} {tyB=BLOCK} f (Question l c xs)    = f (Question l c xs) <+> (query f xs) <+> query f c
  query {tyA=BLOCK} {tyB=BLOCK} f (Solution l c xs)    = f (Solution l c xs) <+> (query f xs) <+> query f c
  query {tyA=BLOCK} {tyB=BLOCK} f (Example l c xs)     = f (Example l c xs) <+> (query f xs) <+> query f c

  query {tyA=BLOCK} {tyB=BLOCK} f block = f block

  -- BLOCK to DOC
  query {tyA=BLOCK} {tyB=DOC} f (Doc t as body) = query f body

  -- INLINE to DOC
  query {tyA=INLINE} {tyB=DOC} f (Doc t as body) = query f body

  -- DOC tp DOC
  query {tyA=DOC} {tyB=DOC} f doc = f doc

  -- BLOCK to SNIPPET
  query {tyA=BLOCK} {tyB=SNIPPET} f (Snippet ss prf) = query f ss

  -- INLINE to SNIPPET
  query {tyA=INLINE} {tyB=SNIPPET} f (Snippet ss prf) = query f ss

  -- snippet to snippet

  query {tyA=SNIPPET} {tyB=SNIPPET} f snip = f snip

-- --------------------------------------------------------------------- [ EOF ]
