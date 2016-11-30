-- ---------------------------------------------------------------- [ Show.idr ]
-- Module    : Show.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Edda.Model.Show

import Edda.Model

%access export
%default partial

implementation Show Step where
  show STAR   = "STAR"
  show PRIME  = "PRIME"

implementation Show QuoteTy where
  show SQuote = "SQuote"
  show DQuote = "DQuote"

implementation Show CiteSty where
  show ParenSty = "ParenCite"
  show TextSty  = "TextCite"

implementation Show ParenTy where
  show Parents = "Parens"
  show Brackets = "Brackets"
  show Braces = "Braces"

implementation Show FontTy where
  show SerifTy = "Serif"
  show SansTy  = "Sans"
  show ScapTy  = "SmallCaps"
  show MonoTy  = "Monospaced"

implementation Show LinkTy where
  show HyperTy   = "HyperLink"
  show ExposedTy = "Exposed"
  show FnoteTy   = "Footnote"
  show RefTy     = "Internal"
  show CiteTy    = "Citation"

implementation Show MarkupTy where
  show BoldTy   = "Strong"
  show EmphTy   = "Emph"
  show StrikeTy = "Strike"
  show UlineTy  = "Uline"

implementation Show RawTy where
  show VerbTy = "Verb"
  show CodeTy = "Code"
  show MathTy = "Math"

implementation Show TextBlockTy where
  show ParaTy        = "PARAGRAPH"
  show TheoremTy     = "THEOREM"
  show CorollaryTy   = "COROLLARY"
  show LemmaTy       = "LEMMA"
  show PropositionTy = "PROPOSITION"
  show ProofTy       = "PROOF"
  show DefinitionTy  = "DEFINITION"
  show ExampleTy     = "EXAMPLE"
  show ExerciseTy    = "EXERCISE"
  show NoteTy        = "NOTE"
  show ProblemTy     = "PROBLEM"
  show QuestionTy    = "QUESTION"
  show RemarkTy      = "REMARK"
  show SolutionTy    = "SOLUTION"
  show QuotationTy   = "QUOTATION"

implementation Show VerbBlockTy where
  show CommentTy  = "COMMENT"
  show ListingTy  = "LISTING"
  show LiteralTy  = "LITERTAL"
  show EquationTy = "EQUATION"

implementation Show ListTy where
  show BulletTy = "Bullet"
  show NumberTy = "Number"

implementation Show EddaTy where
  show INLINE = "INLINE"
  show BLOCK  = "BLOCK"
  show MODEL  = "MODEL"


private partial
showStarInline : Edda STAR INLINE -> String
showStarInline (Punc c)    = unwords ["{Punc", show c,  "}"]
showStarInline (Font ty t) = unwords ["{Font", show ty, show t, "}"]
showStarInline (Raw ty t)  = unwords ["{Raw",  show ty, show t, "}"]

showStarInline (Mark ty ts) = unwords ["{Mark"
                                      , show ty
                                      , concatMap showStarInline ts
                                      , "}"]

showStarInline (Link ty u ts) = unwords ["{Link"
                                        , show ty
                                        , "<" ++ u ++ ">"
                                        , show $ concatMap showStarInline ts
                                        , "}"]


private partial
showPrimeInline : Edda PRIME INLINE -> String
showPrimeInline (Text text) = unwords ["{Text", show text, "}"]
showPrimeInline (Mono mono) = unwords ["{Mono", show mono, "}"]
showPrimeInline (Scap scap) = unwords ["{Scap", show scap, "}"]
showPrimeInline (Sans sans) = unwords ["{Sans", show sans, "}"]

showPrimeInline (Verb verb) = unwords ["{Verb", show verb, "}"]
showPrimeInline (Code code) = unwords ["{Code", show code, "}"]
showPrimeInline (Math math) = unwords ["{Math", show math, "}"]

showPrimeInline (Emph   es) = unwords ["{Emph",   concatMap showPrimeInline es, "}"]
showPrimeInline (Bold   bs) = unwords ["{Strong", concatMap showPrimeInline bs, "}"]
showPrimeInline (Strike ss) = unwords ["{Strike", concatMap showPrimeInline ss, "}"]
showPrimeInline (Uline  us) = unwords ["{Verb",   concatMap showPrimeInline us, "}"]

showPrimeInline (Quote qty ss)  = unwords [ "{" ++ show qty
                                          , concatMap showPrimeInline ss
                                          , "}"]
showPrimeInline (Parens pty ss) = unwords ["{" ++ show pty
                                          , concatMap showPrimeInline ss
                                          , "}"]

showPrimeInline (Ref l)      = unwords ["{Ref", show l, "}"]
showPrimeInline (Cite ty id) = unwords ["{" ++ show ty, id, "}"]
showPrimeInline (Hyper u ds) = unwords ["{Hyper"
                                       , "<" ++ u ++ ">"
                                       , concatMap showPrimeInline ds
                                       , "}"]
showPrimeInline (FNote l ds) = unwords ["{Fnote"
                                       , "<" ++ l ++ ">"
                                       , concatMap showPrimeInline ds
                                       , "}"]

showPrimeInline Space      = "{Space}"
showPrimeInline Newline    = "{Newline}"
showPrimeInline Tab        = "{Tab}"

showPrimeInline LBrace     = "{LBrace}"
showPrimeInline RBrace     = "{RBrace}"

showPrimeInline LParen     = "{LParen}"
showPrimeInline RParen     = "{RParen}"

showPrimeInline LBrack     = "{LBrack}"
showPrimeInline RBrack     = "{RBrack}"
showPrimeInline LAngle     = "{LAngle}"
showPrimeInline RAngle     = "{RAngle}"

showPrimeInline Dollar     = "{Dollar}"
showPrimeInline Colon      = "{Colon}"
showPrimeInline Semi       = "{Semi}"
showPrimeInline EnDash     = "{EnDash}"
showPrimeInline EmDash     = "{EmDash}"
showPrimeInline FSlash     = "{Forwardslash}"
showPrimeInline BSlash     = "{Backslash}"
showPrimeInline Apostrophe = "{Apostrophe}"
showPrimeInline SMark      = "{Speech Mark}"
showPrimeInline Comma      = "{Comma}"
showPrimeInline Plus       = "{Plus}"
showPrimeInline Ellipsis   = "{Ellipsis}"
showPrimeInline Hyphen     = "{Hyphen}"
showPrimeInline Bang       = "{Bang}"
showPrimeInline Period     = "{Period}"
showPrimeInline QMark      = "{Question Mark}"
showPrimeInline Hash       = "{Hash}"
showPrimeInline Equals     = "{Equals}"
showPrimeInline Pipe       = "{Pipe}"

showPrimeInline (MiscPunc c) = "{Punc " ++ show c ++"}"

private
showInline : Edda s INLINE -> String
showInline {s=STAR}  x = showStarInline  x
showInline {s=PRIME} x = showPrimeInline x


private
showStarBlock : Edda STAR BLOCK -> String
showStarBlock (HRule STAR) = "[HRule STAR ]"
showStarBlock (Empty STAR) = "[Empty STAR ]"
showStarBlock (TextBlock ty lab cap as txt) = unwords
    ["[TextBlock"
    , show ty
    , show lab
    , concatMap showStarInline cap
    , show as
    , concatMap showStarInline txt
    , "]"]
showStarBlock (VerbBlock ty lab cap as txt) = unwords
    ["[VerbBlock"
    , show ty
    , show lab
    , concatMap showStarInline cap
    , show as
    , show txt
    , "]"]
showStarBlock (ListBlock ty iis) = unwords
    [ "[BList"
    , show ty
    , concatMap (\is => unwords ["[Item", concatMap showStarInline is, "]"]) iis
    , "]"]
showStarBlock (Section _ d l t a) = unwords
    [ "[Heading STAR"
    , show d
    , show l
    , concatMap showStarInline t
    , show a
    , "]"]
showStarBlock (Figure _ l c as img) = unwords
    [ "[FigBlock STAR"
    , show l
    , concatMap showStarInline c
    , show as
    , showStarInline img
    , "]"]
showStarBlock (DList _ ds) = unwords
    [ "[DList STAR"
    , concatMap (\(k,vs) => concatMap showStarInline k ++ " " ++ concatMap showStarInline vs) ds
    , "]"]


private
showPrimeLBlock : String -> List (List (Edda PRIME INLINE)) -> String
showPrimeLBlock tag is = unwords
    [ "[" ++ tag
    , concatMap showItem is
    , "]"]
  where
    showItem : List (Edda PRIME INLINE) -> String
    showItem xs = unwords ["[Item"
                          , concatMap showPrimeInline xs
                          , "]"]

showPrimeQBlock : String
               -> Maybe String
               -> List (Edda PRIME INLINE)
               -> List (Edda PRIME INLINE)
               -> String
showPrimeQBlock tag l tit quo = unwords
    [ "[" ++ tag
    , show l
    , concatMap showPrimeInline tit
    , concatMap showPrimeInline quo
    , "]"]

private
showPrimeBlock : Edda PRIME BLOCK -> String
showPrimeBlock (HRule _) = "[HRule PRIME]"
showPrimeBlock (Empty _) = "[Empty PRIME]"

showPrimeBlock (Section _ d l t a) = unwords
    ["[Heading PRIME"
    , show d
    , show l
    , concatMap showPrimeInline t
    , show a
    , "]"]

showPrimeBlock (Figure _ l c as img) = unwords
    [ "[FigBlock PRIME"
    , show l
    , concatMap showPrimeInline c
    , show as
    , showPrimeInline img
    , "]"]

showPrimeBlock (DList _ ds) = unwords
    [ "[DList PRIME"
    , concatMap (\(k,vs) => concatMap showPrimeInline k ++ " " ++ concatMap showPrimeInline vs) ds
    , "]"]

showPrimeBlock (BList is) = showPrimeLBlock "BList"  is
showPrimeBlock (OList is) = showPrimeLBlock "OList"  is
showPrimeBlock (Para txt) = unwords
    [ "[Para"
    , concatMap showPrimeInline txt
    , "]"]

showPrimeBlock (Comment cs)   = unwords ["[Comment", show cs, "]"]
showPrimeBlock (Equation l m) = unwords [ "[EqBlock", show l, show m, "]"]
showPrimeBlock (Quotation l qs) = unwords
    ["[BQuote"
    , show l
    , concatMap showPrimeInline qs
    , "]"]

showPrimeBlock (Literal l cap src) = unwords
    [ "[Literal"
    , show l   ++ " "
    , concatMap showPrimeInline cap
    , show src
    , "]"]

showPrimeBlock (Listing l cap lang ops as co) = unwords
    [ "[CodeBlock"
    , show l
    , concatMap showPrimeInline cap
    , show lang
    , show ops
    , show as
    , show co
    , "]"]

showPrimeBlock (Theorem l c txt)     = showPrimeQBlock "Theorem"     l c txt
showPrimeBlock (Corollary l c txt)   = showPrimeQBlock "Corollary"   l c txt
showPrimeBlock (Lemma l c txt)       = showPrimeQBlock "Lemma"       l c txt
showPrimeBlock (Proposition l c txt) = showPrimeQBlock "Proposition" l c txt
showPrimeBlock (Proof l c txt)       = showPrimeQBlock "Proof"       l c txt
showPrimeBlock (Definition l c txt)  = showPrimeQBlock "Definition"  l c txt
showPrimeBlock (Exercise l c txt)    = showPrimeQBlock "Exercise"    l c txt
showPrimeBlock (Note l c txt)        = showPrimeQBlock "Note"        l c txt
showPrimeBlock (Remark l c txt)      = showPrimeQBlock "Remark"      l c txt
showPrimeBlock (Problem l c txt)     = showPrimeQBlock "Problem"     l c txt
showPrimeBlock (Question l c txt)    = showPrimeQBlock "Question"    l c txt
showPrimeBlock (Solution l c txt)    = showPrimeQBlock "Solution"    l c txt
showPrimeBlock (Example l c txt)     = showPrimeQBlock "Example"     l c txt

private
showBlock : Edda s BLOCK -> String
showBlock {s=STAR}  x = showStarBlock  x
showBlock {s=PRIME} x = showPrimeBlock x

private
showStarModel : Edda STAR MODEL -> String
showStarModel (EddaRaw ps body) = unwords
    [ "[Edda STAR"
    , "[Mdata " ++ concatMap show ps ++ "]"
    , concatMap showStarBlock body
    , "]"]

private
showPrimeModel : Edda PRIME MODEL -> String
showPrimeModel (MkEdda ps body) = unwords
    [ "[Edda PRIME"
    , "[Mdata " ++ concatMap show ps ++ "]"
    , concatMap showPrimeBlock  body
    , "]"]

private
showModel : Edda s MODEL -> String
showModel {s=STAR}  x = showStarModel x
showModel {s=PRIME} x = showPrimeModel x

implementation Show (Edda s ty) where
  show {ty=INLINE} x = showInline x
  show {ty=BLOCK}  x = showBlock  x
  show {ty=MODEL}  x = showModel  x

-- --------------------------------------------------------------------- [ EOF ]
