module Edda.Model.Eq

import Edda.Model

instance Eq Step where
    (==) Star  Star  = True
    (==) Prime Prime = True
    (==) _     _     = False

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

mutual
  %assert_total -- @ TODO check if okay to do this?
  inlineEq : Inline s -> Inline s -> Bool
  inlineEq (Link xty xu xdesc) (Link yty yu ydesc) = xty == yty && xu == yu && xdesc == ydesc
  inlineEq (Mark xty xdesc) (Mark yty ydesc) = xty == yty && xdesc == ydesc
  inlineEq (Raw xty xdesc)  (Raw yty ydesc)  = xty == yty && xdesc == ydesc
  inlineEq (Font xty xtxt)  (Font yty ytxt)  = xty == yty && xtxt == ytxt
  inlineEq (Punc c)         (Punc d)         = c == d

  inlineEq (Text x) (Text y) = x == y
  inlineEq (Sans x) (Sans y) = x == y
  inlineEq (Scap x) (Scap y) = x == y
  inlineEq (Mono x) (Mono y) = x == y

  inlineEq (Verb x) (Verb y) = x == y
  inlineEq (Code x) (Code y) = x == y
  inlineEq (Math x) (Math y) = x == y

  inlineEq (Emph   xs) (Emph   ys) = xs == ys
  inlineEq (Bold   xs) (Bold   ys) = xs == ys
  inlineEq (Strike xs) (Strike ys) = xs == ys
  inlineEq (Uline  xs) (Uline  ys) = xs == ys

  inlineEq (Quote xty xs) (Quote yty ys) = xty == yty && xs == ys
  inlineEq (Parens xty xs) (Parens yty ys) = xty == yty && xs ==ys

  inlineEq (Ref x)       (Ref y)       = x == y
  inlineEq (Cite xty xs) (Cite yty ys) = xty == yty && xs == ys
  inlineEq (Hyper x xs)  (Hyper y ys)  = x == y && xs == ys
  inlineEq (FNote x xs)  (FNote y ys)  = x == y && xs == ys

  inlineEq Space        Space      = True
  inlineEq Newline      Newline    = True
  inlineEq Tab          Tab        = True
  inlineEq LAngle       LAngle     = True
  inlineEq RAngle       RAngle     = True
  inlineEq Colon        Colon      = True
  inlineEq Semi         Semi       = True
  inlineEq FSlash       FSlash     = True
  inlineEq BSlash       BSlash     = True
  inlineEq Apostrophe   Apostrophe = True
  inlineEq SMark        SMark      = True
  inlineEq Hyphen       Hyphen     = True
  inlineEq Comma        Comma      = True
  inlineEq Plus         Plus       = True
  inlineEq Bang         Bang       = True
  inlineEq Period       Period     = True
  inlineEq QMark        QMark      = True
  inlineEq Hash         Hash       = True
  inlineEq Equals       Equals     = True
  inlineEq Dollar       Dollar     = True
  inlineEq Pipe         Pipe       = True
  inlineEq Ellipsis     Ellipsis   = True
  inlineEq EmDash       EmDash     = True
  inlineEq EnDash       EnDash     = True

  inlineEq LBrace       LBrace     = True
  inlineEq RBrace       RBrace     = True
  inlineEq LParen       LParen     = True
  inlineEq RParen       RParen     = True
  inlineEq LBrack       LBrack     = True
  inlineEq RBrack       RBrack     = True

  inlineEq (MiscPunc c) (MiscPunc d) = c == d
  inlineEq _ _ = False

  instance Eq (Inline s) where
      (==) = inlineEq

instance Eq TAlign where
    (==) AlignLeft   AlignLeft   = True
    (==) AlignRight  AlignRight  = True
    (==) AlignCenter AlignCenter = True
    (==) AlignPar    AlignPar    = True
    (==) _           _           = False

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

instance Eq Tabular where -- @TODO Make deep
  (==) (MkTabular xa xc) (MkTabular ya yc) = True
  (==) _                 _                 = False

mutual
  blockEq : Block s -> Block s -> Bool
  blockEq (ListBlock xty xs)           (ListBlock yty ys)           = xty == yty && xs == ys
  blockEq (TextBlock xty xl xc xas xs) (TextBlock yty yl yc yas ys) = xty == yty && xl == yl && xc == yc && xas == yas && xs == ys
  blockEq (VerbBlock xty xl xc xas x)  (VerbBlock yty yl yc yas y)  = xty == yty && xl == yl && xc == yc && xas == yas && x == y

  blockEq {s} (HRule s)              (HRule s)              = True
  blockEq {s} (Empty s)              (Empty s)              = True
  blockEq {s} (Header s x xl xt)     (Header s y yl yt)     = x == y && xl == yl && xt == yt
  blockEq {s} (Figure s xl xc xas x) (Figure s yl yc yas y) = xl == yl && xc == yc && xas == yas && x == y
  blockEq {s} (Table s xl xc x)      (Table s yl yc y)      = xl == yl && xc == yc && x == y
  blockEq {s} (DList s xs)           (DList s ys)           = xs == ys

  blockEq (OList xs) (OList ys) = xs == ys
  blockEq (BList xs) (BList ys) = xs == ys

  blockEq (Comment xs)       (Comment ys)       = xs == ys
  blockEq (Equation xl x)    (Equation yl y)    = xl == yl  && x == y
  blockEq (Literal xl xc xs) (Literal yl yc ys) = xl == yl && xc == yc && xs == ys

  blockEq (Listing xl xc xty xops xas x) (Listing yl yc yty yops yas y) = xl == yl && xc == yc && xty == yty && xops == yops && xas == yas && x == y

  blockEq (Para xs)         (Para ys)         = xs == ys
  blockEq (Quotation xl xs) (Quotation yl ys) = xl ==yl && xs == ys

  blockEq (Theorem xl xc xs)     (Theorem yl yc ys)     = xl == yl && xc == yc && xs == ys
  blockEq (Corollary xl xc xs)   (Corollary yl yc ys)   = xl == yl && xc == yc && xs == ys
  blockEq (Lemma xl xc xs)       (Lemma yl yc ys)       = xl == yl && xc == yc && xs == ys
  blockEq (Proposition xl xc xs) (Proposition yl yc ys) = xl == yl && xc == yc && xs == ys
  blockEq (Proof xl xc xs)       (Proof yl yc ys)       = xl == yl && xc == yc && xs == ys
  blockEq (Definition xl xc xs)  (Definition yl yc ys)  = xl == yl && xc == yc && xs == ys
  blockEq (Exercise xl xc xs)    (Exercise yl yc ys)    = xl == yl && xc == yc && xs == ys
  blockEq (Note xl xc xs)        (Note yl yc ys)        = xl == yl && xc == yc && xs == ys
  blockEq (Remark xl xc xs)      (Remark yl yc ys)      = xl == yl && xc == yc && xs == ys
  blockEq (Problem xl xc xs)     (Problem yl yc ys)     = xl == yl && xc == yc && xs == ys
  blockEq (Question xl xc xs)    (Question yl yc ys)    = xl == yl && xc == yc && xs == ys
  blockEq (Solution xl xc xs)    (Solution yl yc ys)    = xl == yl && xc == yc && xs == ys
  blockEq (Example xl xc xs)     (Example yl yc ys)     = xl == yl && xc == yc && xs == ys

  blockEq _ _ = False

  instance Eq (Block s) where
      (==) = blockEq


eddaEq : Edda s -> Edda s -> Bool
eddaEq {s} (MkEdda s x xs) (MkEdda s y ys) = x == y && xs == ys

instance Eq (Edda s) where
    (==) = eddaEq
