module Edda.Model.Eq

import Edda.Model

instance Eq Step where
    (==) STAR  STAR  = True
    (==) PRIME PRIME = True
    (==) _     _     = False

instance Eq EddaTy where
    (==) INLINE INLINE = True
    (==) BLOCK  BLOCK  = True
    (==) MODEL  MODEL  = True
    (==) _      _      = False

instance Eq FontTy where
    (==) SerifTy SerifTy = True
    (==) SansTy  SansTy  = True
    (==) ScapTy  ScapTy  = True
    (==) MonoTy  MonoTy  = True
    (==) _       _       = False

instance Eq QuoteTy where
    (==) SQuote SQuote = True
    (==) DQuote DQuote = True
    (==) _      _      = False

instance Eq CiteSty where
    (==) ParenSty ParenSty = True
    (==) TextSty  TextSty  = True
    (==) _        _        = False

instance Eq ParenTy where
    (==) Parents  Parents  = True
    (==) Brackets Brackets = True
    (==) Braces   Braces   = True
    (==) _        _        = False

instance Eq LinkTy where
    (==) HyperTy   HyperTy   = True
    (==) ExposedTy ExposedTy = True
    (==) FnoteTy   FnoteTy   = True
    (==) RefTy     RefTy     = True
    (==) CiteTy    CiteTy    = True
    (==) _         _         = False

instance Eq MarkupTy where
    (==) BoldTy   BoldTy   = True
    (==) EmphTy   EmphTy   = True
    (==) StrikeTy StrikeTy = True
    (==) UlineTy  UlineTy  = True
    (==) _        _        = False

instance Eq RawTy where
    (==) VerbTy VerbTy = True
    (==) CodeTy CodeTy = True
    (==) MathTy MathTy = True
    (==) _      _      = False

instance Eq VerbBlockTy where
    (==) CommentTy  CommentTy  = True
    (==) ListingTy  ListingTy  = True
    (==) LiteralTy  LiteralTy  = True
    (==) EquationTy EquationTy = True
    (==) _          _          = False

instance Eq TextBlockTy where
    (==) ParaTy         ParaTy        = True
    (==) TheoremTy      TheoremTy     = True
    (==) CorollaryTy    CorollaryTy   = True
    (==) LemmaTy        LemmaTy       = True
    (==) PropositionTy  PropositionTy = True
    (==) ProofTy        ProofTy       = True
    (==) DefinitionTy   DefinitionTy  = True

    (==) ExerciseTy     ExerciseTy    = True
    (==) NoteTy         NoteTy        = True
    (==) ProblemTy      ProblemTy     = True
    (==) QuestionTy     QuestionTy    = True
    (==) RemarkTy       RemarkTy      = True

    (==) SolutionTy     SolutionTy    = True
    (==) ExampleTy      ExampleTy     = True
    (==) QuotationTy    QuotationTy   = True
    (==) _              _             = False

instance Eq ListTy where
  (==) BulletTy BulletTy = True
  (==) NumberTy NumberTy = True
  (==) _        _        = False


mutual
  %assert_total
  eqEdda : Edda s ty -> Edda s' ty' -> Bool
  eqEdda (Link xty xu xdesc) (Link yty yu ydesc) = xty == yty && xu == yu && xdesc == ydesc
  eqEdda (Mark xty xdesc) (Mark yty ydesc) = xty == yty && xdesc == ydesc
  eqEdda (Raw xty xdesc)  (Raw yty ydesc)  = xty == yty && xdesc == ydesc
  eqEdda (Font xty xtxt)  (Font yty ytxt)  = xty == yty && xtxt == ytxt
  eqEdda (Punc c)         (Punc d)         = c == d

  eqEdda (Text x) (Text y) = x == y
  eqEdda (Sans x) (Sans y) = x == y
  eqEdda (Scap x) (Scap y) = x == y
  eqEdda (Mono x) (Mono y) = x == y

  eqEdda (Verb x) (Verb y) = x == y
  eqEdda (Code x) (Code y) = x == y
  eqEdda (Math x) (Math y) = x == y

  eqEdda (Emph   xs) (Emph   ys) = xs == ys
  eqEdda (Bold   xs) (Bold   ys) = xs == ys
  eqEdda (Strike xs) (Strike ys) = xs == ys
  eqEdda (Uline  xs) (Uline  ys) = xs == ys

  eqEdda (Quote xty xs) (Quote yty ys) = xty == yty && xs == ys
  eqEdda (Parens xty xs) (Parens yty ys) = xty == yty && xs ==ys

  eqEdda (Ref x)       (Ref y)       = x == y
  eqEdda (Cite xty xs) (Cite yty ys) = xty == yty && xs == ys
  eqEdda (Hyper x xs)  (Hyper y ys)  = x == y && xs == ys
  eqEdda (FNote x xs)  (FNote y ys)  = x == y && xs == ys

  eqEdda Space        Space      = True
  eqEdda Newline      Newline    = True
  eqEdda Tab          Tab        = True
  eqEdda LAngle       LAngle     = True
  eqEdda RAngle       RAngle     = True
  eqEdda Colon        Colon      = True
  eqEdda Semi         Semi       = True
  eqEdda FSlash       FSlash     = True
  eqEdda BSlash       BSlash     = True
  eqEdda Apostrophe   Apostrophe = True
  eqEdda SMark        SMark      = True
  eqEdda Hyphen       Hyphen     = True
  eqEdda Comma        Comma      = True
  eqEdda Plus         Plus       = True
  eqEdda Bang         Bang       = True
  eqEdda Period       Period     = True
  eqEdda QMark        QMark      = True
  eqEdda Hash         Hash       = True
  eqEdda Equals       Equals     = True
  eqEdda Dollar       Dollar     = True
  eqEdda Pipe         Pipe       = True
  eqEdda Ellipsis     Ellipsis   = True
  eqEdda EmDash       EmDash     = True
  eqEdda EnDash       EnDash     = True

  eqEdda LBrace       LBrace     = True
  eqEdda RBrace       RBrace     = True
  eqEdda LParen       LParen     = True
  eqEdda RParen       RParen     = True
  eqEdda LBrack       LBrack     = True
  eqEdda RBrack       RBrack     = True

  eqEdda (MiscPunc c) (MiscPunc d) = c == d

  eqEdda (ListBlock xty xs)            (ListBlock yty ys)           = xty == yty && xs == ys
  eqEdda (TextBlock xty xl xc xas xs)  (TextBlock yty yl yc yas ys) = xty == yty && xl == yl && xc == yc && xas == yas && xs == ys
  eqEdda (VerbBlock xty xl xc xas x)   (VerbBlock yty yl yc yas y)  = xty == yty && xl == yl && xc == yc && xas == yas && x == y

  eqEdda {s} (HRule s)              (HRule s)                 = True
  eqEdda {s} (Empty s)              (Empty s)                 = True
  eqEdda {s} (Section s x xl xt xa) (Section s y yl yt ya) = x == y && xl == yl && xt == yt && xa == ya
  eqEdda {s} (Figure s xl xc xas x) (Figure s yl yc yas y) = xl == yl && xc == yc && xas == yas && x == y
  eqEdda {s} (DList s xs)           (DList s ys)           = xs == ys

  eqEdda (OList xs) (OList ys) = xs == ys
  eqEdda (BList xs) (BList ys) = xs == ys

  eqEdda (Comment xs)       (Comment ys)       = xs == ys
  eqEdda (Equation xl x)    (Equation yl y)    = xl == yl  && x == y
  eqEdda (Literal xl xc xs) (Literal yl yc ys) = xl == yl && xc == yc && xs == ys

  eqEdda (Listing xl xc xty xops xas x) (Listing yl yc yty yops yas y) = xl == yl && xc == yc && xty == yty && xops == yops && xas == yas && x == y

  eqEdda (Para xs)         (Para ys)         = xs == ys
  eqEdda (Quotation xl xs) (Quotation yl ys) = xl ==yl && xs == ys

  eqEdda (Theorem xl xc xs)     (Theorem yl yc ys)     = xl == yl && xc == yc && xs == ys
  eqEdda (Corollary xl xc xs)   (Corollary yl yc ys)   = xl == yl && xc == yc && xs == ys
  eqEdda (Lemma xl xc xs)       (Lemma yl yc ys)       = xl == yl && xc == yc && xs == ys
  eqEdda (Proposition xl xc xs) (Proposition yl yc ys) = xl == yl && xc == yc && xs == ys
  eqEdda (Proof xl xc xs)       (Proof yl yc ys)       = xl == yl && xc == yc && xs == ys
  eqEdda (Definition xl xc xs)  (Definition yl yc ys)  = xl == yl && xc == yc && xs == ys
  eqEdda (Exercise xl xc xs)    (Exercise yl yc ys)    = xl == yl && xc == yc && xs == ys
  eqEdda (Note xl xc xs)        (Note yl yc ys)        = xl == yl && xc == yc && xs == ys
  eqEdda (Remark xl xc xs)      (Remark yl yc ys)      = xl == yl && xc == yc && xs == ys
  eqEdda (Problem xl xc xs)     (Problem yl yc ys)     = xl == yl && xc == yc && xs == ys
  eqEdda (Question xl xc xs)    (Question yl yc ys)    = xl == yl && xc == yc && xs == ys
  eqEdda (Solution xl xc xs)    (Solution yl yc ys)    = xl == yl && xc == yc && xs == ys
  eqEdda (Example xl xc xs)     (Example yl yc ys)     = xl == yl && xc == yc && xs == ys

  eqEdda (EddaRaw x xs) (EddaRaw y ys) = x == y && xs == ys
  eqEdda (MkEdda x xs)  (MkEdda y ys)  = x == y && xs == ys

  eqEdda _ _ = False

  instance Eq (Edda s ty) where
      (==) = eqEdda
-- --------------------------------------------------------------------- [ EOF ]
