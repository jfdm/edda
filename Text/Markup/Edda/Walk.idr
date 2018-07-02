module Text.Markup.Edda.Walk

import Data.AVL.Dict

import Text.Markup.Edda.Model.Common
import Text.Markup.Edda.Model.Raw
import Text.Markup.Edda.Model.Processed

%access export

interface Walkable a z where
  walk : (a -> a) -> z -> z

Walkable a b => Walkable a (List b) where
  walk f xs = map (walk f) xs

(Walkable a b, Walkable a c) => Walkable a (b,c) where
  walk f (x,y) = (walk f x, walk f y)


Walkable (Edda tyA) (Edda tyB) where
  -- inline to inline
  walk {tyA=INLINE} {tyB=INLINE} f (Emph xs)   = f $ Emph (walk f xs)
  walk {tyA=INLINE} {tyB=INLINE} f (Bold xs)   = f $ Bold (walk f xs)
  walk {tyA=INLINE} {tyB=INLINE} f (Strike xs) = f $ Strike (walk f xs)
  walk {tyA=INLINE} {tyB=INLINE} f (Uline xs)  = f $ Uline (walk f xs)

  walk {tyA=INLINE} {tyB=INLINE} f (Quote ty xs)  = f $ Quote ty (walk f xs)
  walk {tyA=INLINE} {tyB=INLINE} f (Parens ty xs) = f $ Parens ty (walk f xs)

  walk {tyA=INLINE} {tyB=INLINE} f (Ref uri)        = f $ Ref uri
  walk {tyA=INLINE} {tyB=INLINE} f (Cite ty uri)    = f $ Cite ty uri
  walk {tyA=INLINE} {tyB=INLINE} f (Hyper uri desc) = f $ Hyper uri (walk f desc)
  walk {tyA=INLINE} {tyB=INLINE} f (FNote uri desc) = f $ FNote uri (walk f desc)
  walk {tyA=INLINE} {tyB=INLINE} f (MiscPunc c)     = f $ MiscPunc c

  walk {tyA=INLINE} {tyB=INLINE} f inline = f inline

  -- Inline structures from Blocks
  walk {tyA=INLINE} {tyB=BLOCK} f (Section d l t as bs) = Section d l (walk f t) as (walk f bs)
  walk {tyA=INLINE} {tyB=BLOCK} f (Figure l c as fig)   = Figure l (walk f c) as (walk f fig)
  walk {tyA=INLINE} {tyB=BLOCK} f (DList kvs)           = DList (walk f kvs)

  walk {tyA=INLINE} {tyB=BLOCK} f (OList xs) = OList (walk f xs)
  walk {tyA=INLINE} {tyB=BLOCK} f (BList xs) = BList (walk f xs)

  walk {tyA=INLINE} {tyB=BLOCK} f (Literal l c ss)       = Literal l (walk f c) ss
  walk {tyA=INLINE} {tyB=BLOCK} f (Listing l c ty ops as s)  = Listing l (walk f c) ty ops as s

  walk {tyA=INLINE} {tyB=BLOCK} f (Para xs)        = Para $ walk f xs
  walk {tyA=INLINE} {tyB=BLOCK} f (Quotation l xs) = Quotation l (walk f xs)

  walk {tyA=INLINE} {tyB=BLOCK} f (Theorem l c xs)     = Theorem l (walk f c) (walk f xs)
  walk {tyA=INLINE} {tyB=BLOCK} f (Corollary l c xs)   = Corollary l (walk f c) (walk f xs)
  walk {tyA=INLINE} {tyB=BLOCK} f (Lemma l c xs)       = Lemma l (walk f c) (walk f xs)
  walk {tyA=INLINE} {tyB=BLOCK} f (Proposition l c xs) = Proposition l (walk f c) (walk f xs)
  walk {tyA=INLINE} {tyB=BLOCK} f (Proof l c xs)       = Proof l (walk f c) (walk f xs)
  walk {tyA=INLINE} {tyB=BLOCK} f (Definition l c xs)  = Definition l (walk f c) (walk f xs)
  walk {tyA=INLINE} {tyB=BLOCK} f (Exercise l c xs)    = Exercise l (walk f c) (walk f xs)
  walk {tyA=INLINE} {tyB=BLOCK} f (Note l c xs)        = Note l (walk f c) (walk f xs)
  walk {tyA=INLINE} {tyB=BLOCK} f (Remark l c xs)      = Remark l (walk f c) (walk f xs)
  walk {tyA=INLINE} {tyB=BLOCK} f (Problem l c xs)     = Problem l (walk f c) (walk f xs)
  walk {tyA=INLINE} {tyB=BLOCK} f (Question l c xs)    = Question l (walk f c) (walk f xs)
  walk {tyA=INLINE} {tyB=BLOCK} f (Solution l c xs)    = Solution l (walk f c) (walk f xs)
  walk {tyA=INLINE} {tyB=BLOCK} f (Example l c xs)     = Example l (walk f c) (walk f xs)

  walk {tyA=INLINE} {tyB=BLOCK} f block = block

  walk {tyA=BLOCK} {tyB=INLINE} f (Emph xs)   = Emph (walk f xs)
  walk {tyA=BLOCK} {tyB=INLINE} f (Bold xs)   = Bold (walk f xs)
  walk {tyA=BLOCK} {tyB=INLINE} f (Strike xs) = Strike (walk f xs)
  walk {tyA=BLOCK} {tyB=INLINE} f (Uline xs)  = Uline (walk f xs)

  walk {tyA=BLOCK} {tyB=INLINE} f (Quote ty xs)  = Quote ty (walk f xs)
  walk {tyA=BLOCK} {tyB=INLINE} f (Parens ty xs) = Parens ty (walk f xs)

  walk {tyA=BLOCK} {tyB=INLINE} f (Hyper l xs) = Hyper l (walk f xs)
  walk {tyA=BLOCK} {tyB=INLINE} f (FNote l xs) = FNote l (walk f xs)
  walk {tyA=BLOCK} {tyB=INLINE} f inline = inline

  -- Walk Block structures from Blocks
  walk {tyA=BLOCK} {tyB=BLOCK} f (Section d l t as bs)  = f $ Section d l (walk f t) as (walk f bs)
  walk {tyA=BLOCK} {tyB=BLOCK} f (Figure l c as fig) = f $ Figure l (walk f c) as (walk f fig)
  walk {tyA=BLOCK} {tyB=BLOCK} f (DList kvs)         = f $ DList (walk f kvs)

  walk {tyA=BLOCK} {tyB=BLOCK} f (OList xs)  = f $ OList (walk f xs)
  walk {tyA=BLOCK} {tyB=BLOCK} f (BList xs)  = f $ BList (walk f xs)

  walk {tyA=BLOCK} {tyB=BLOCK} f (Para xs)           = f $ Para $ walk f xs
  walk {tyA=BLOCK} {tyB=BLOCK} f (Quotation l xs)    = f $ Quotation l (walk f xs)

  walk {tyA=BLOCK} {tyB=BLOCK} f (Theorem l c xs)     = f $ Theorem l c (walk f xs)
  walk {tyA=BLOCK} {tyB=BLOCK} f (Corollary l c xs)   = f $ Corollary l c (walk f xs)
  walk {tyA=BLOCK} {tyB=BLOCK} f (Lemma l c xs)       = f $ Lemma l c (walk f xs)
  walk {tyA=BLOCK} {tyB=BLOCK} f (Proposition l c xs) = f $ Proposition l c (walk f xs)
  walk {tyA=BLOCK} {tyB=BLOCK} f (Proof l c xs)       = f $ Proof l c (walk f xs)
  walk {tyA=BLOCK} {tyB=BLOCK} f (Definition l c xs)  = f $ Definition l c (walk f xs)
  walk {tyA=BLOCK} {tyB=BLOCK} f (Exercise l c xs)    = f $ Exercise l c (walk f xs)
  walk {tyA=BLOCK} {tyB=BLOCK} f (Note l c xs)        = f $ Note l c (walk f xs)
  walk {tyA=BLOCK} {tyB=BLOCK} f (Remark l c xs)      = f $ Remark l c (walk f xs)
  walk {tyA=BLOCK} {tyB=BLOCK} f (Problem l c xs)     = f $ Problem l c (walk f xs)
  walk {tyA=BLOCK} {tyB=BLOCK} f (Question l c xs)    = f $ Question l c (walk f xs)
  walk {tyA=BLOCK} {tyB=BLOCK} f (Solution l c xs)    = f $ Solution l c (walk f xs)
  walk {tyA=BLOCK} {tyB=BLOCK} f (Example l c xs)     = f $ Example l c (walk f xs)

  walk {tyA=BLOCK} {tyB=BLOCK} f block  = f block

  -- Walk block structures from Doc
  walk {tyA=BLOCK} {tyB=DOC} f (Doc t as bs) = Doc t as (walk f bs)

  -- Walk Inline structures from Doc
  walk {tyA=INLINE} {tyB=DOC} f (Doc t as bs) = Doc (walk f t) as (walk f bs)

  -- Walk the doc
  walk {tyA=DOC} {tyB=DOC} f x = f x

  -- Walk block structures from Doc
  walk {tyA=BLOCK} {tyB=SNIPPET} f (Snippet ss prf) = Snippet (walk f ss) prf

  -- Walk Inline structures from Doc
  walk {tyA=INLINE} {tyB=SNIPPET} f (Snippet ss prf) = Snippet (walk f ss) prf

  -- Walk the doc
  walk {tyA=SNIPPET} {tyB=SNIPPET} f x = f x

-- --------------------------------------------------------------------- [ EOF ]
