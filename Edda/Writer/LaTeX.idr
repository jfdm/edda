-- --------------------------------------------------------------- [ LaTeX.idr ]
-- Module    : LaTeX.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Edda.Writer.LaTeX

import Effects
import Effect.File
import Effect.Exception

import Edda.Model
import Edda.Utils

import Edda.Writer.Common

-- -------------------------------------------------------------- [ Directives ]

%access private

-- ------------------------------------------------------------ [ Misc Writing ]

macro : String -> String -> String
macro c s = concat ["\\", c, "{", s, "}"]

-- ----------------------------------------------------------- [ Write Inlines ]

mutual
  inlines : List (Edda PRIME INLINE) -> String
  inlines xs = concatMap inline xs

  parens : String -> String -> List (Edda PRIME INLINE) -> String
  parens l r ts = concat [l, inlines ts, r]

  link : String -> List (Edda PRIME INLINE) -> String
  link uri Nil  = macro "url"  uri
  link uri desc = concat [macro "href" uri, "{", inlines desc, "}"]

  inline : Edda PRIME INLINE -> String
  inline (Text t) = t
  inline (Sans t) = macro "textsf" t
  inline (Scap t) = macro "textsc" t
  inline (Mono t) = macro "texttt" t
  inline (Verb v)     = "\\verb!" ++ v ++ "!"
  inline (Code v)     = macro "texttt" v
  inline (Math v)     = "$" ++ v ++ "$"
  inline (Emph t)     = macro "emph"      (inlines t)
  inline (Bold t)     = macro "textbf"    (inlines t )
  inline (Strike t)   = macro "sout"      (inlines t)
  inline (Uline t)    = macro "underline" (inlines t)
  inline (Quote ty t) =
    case ty of
      SQuote => macro "squote" (inlines t)
      DQuote => macro "dquote" (inlines t)
  inline (Parens ty t) =
    case ty of
      Parents  => parens "("   ")"   t
      Brackets => parens "["   "]"   t
      Braces   => parens "\\{" "\\}" t
  inline (Ref url)        = link url Nil
  inline (Hyper uri desc) = link uri desc
  inline (FNote l d)   = macro "footnote" (inlines d)
  inline (Cite ty uri) =
    case ty of
      ParenSty => (macro "cite"  uri)
      TextSty  => (macro "citet" uri)
  inline (MiscPunc c) =
    case c of
      '%' => "\\%"
      '_' => "\\_"
      '^' => "\\^"
      '~' => "\\~"
      c   => cast c
  inline Space      = " "
  inline Newline    = "\n"
  inline Tab        = "\t"
  inline LBrace     = "\\{"
  inline RBrace     = "\\}"
  inline LParen     = "("
  inline RParen     = ")"
  inline LBrack     = "["
  inline RBrack     = "]"
  inline LAngle     = "\\textless"
  inline RAngle     = "\\textgreater"
  inline Dollar     = "\\$"
  inline Colon      = ":"
  inline Semi       = ";"
  inline EnDash     = "--"
  inline EmDash     = "---"
  inline FSlash     = "/"
  inline BSlash     = "\\"
  inline Apostrophe = "'"
  inline SMark      = "\""
  inline Comma      = ","
  inline Plus       = "+"
  inline Ellipsis   = "\\ldots"
  inline Hyphen     = "-"
  inline Bang       = "!"
  inline Period     = "."
  inline QMark      = "?"
  inline Hash       = "\\#"
  inline Equals     = "="
  inline Pipe       = "$\\mid$"

-- ----------------------------------------------------- [ Write Generic Block ]
env : String -> String -> String
env n body = unlines
    [ macro "begin" (toLower n)
    , body
    , macro "end" (toLower n)]


thm : String
   -> Maybe String
   -> List (Edda PRIME INLINE)
   -> List (Edda PRIME INLINE)
   -> String
thm e l c b = env (toLower e) body
  where
    body : String
    body = unlines
      [ "[" ++ inlines c ++ "]"
      , strFromMaybe (macro "label") l
      , inlines b
      ]

figure : Maybe String
      -> List (Edda PRIME INLINE)
      -> String
      -> String
figure l c body = env ("figure") body'
  where
    body' : String
    body' = unlines
      [ body
      , macro "caption" $ inlines c
      , macro "label"   $ fromMaybe "missing" l
      ]


-- Make generic
list : String -> List (List (Edda PRIME INLINE)) -> String
list s is = env s (unlines $ map item is)
  where
    item : List (Edda PRIME INLINE) -> String
    item bs = unwords ["\\item", inlines bs, "\n"]

dlist : List (Pair (List (Edda PRIME INLINE)) (List (Edda PRIME INLINE))) -> String
dlist kvs = env "description" (unlines $ map descItem kvs)
  where
    descItem : (List (Edda PRIME INLINE), List (Edda PRIME INLINE)) -> String
    descItem (k,v) = unwords ["\\item[" ++ inlines k ++ "]", inlines v]

secLvl : Nat -> List (Edda PRIME INLINE) -> String
secLvl Z                 t = macro "section"       (inlines t)
secLvl (S Z)             t = macro "subsection"    (inlines t)
secLvl (S (S Z))         t = macro "subsubsection" (inlines t)
secLvl (S (S (S Z)))     t = macro "paragraph"     (inlines t)
secLvl (S (S (S (S Z)))) t = macro "subparagraph"  (inlines t)
secLvl _                 t = inlines t ++ " % Depth not recognised\n"

-- ------------------------------------------------------------- [ Write Block ]
-- deal with attrs
||| Write block to LaTeX version.
export
block : Edda PRIME BLOCK -> String
block (HRule PRIME) = "\\hrulefill"
block (Empty PRIME) = "\n"
block (Section PRIME lvl label title as) =
    unwords [ secLvl lvl title
            , strFromMaybe (macro "label") label
            , "\n"]
block (Figure PRIME l c as fig) = figure (Just l) c (inline fig) ++ "\n"
block (DList PRIME kvs)         = dlist kvs
block (OList bs)                = list "itemize"  bs
block (BList bs)                = list "enumerate" bs

block (Para txt) = inlines txt ++ "\n\n"

block (Listing l c lang langopts as src) = figure l c (env "verbatim" src)

block (Comment ss)          = env "comment" ss
block (Equation l eq)       = env "equation" eq  -- Add support for caption and label
block (Literal l c src)     = env "verbatim" src -- Add support for caption and label

block (Quotation l txt)     = thm "QUOTE" l Nil txt
block (Theorem l c txt)     = thm "Theorem" l c txt
block (Corollary l c txt)   = thm "COROLLARY" l c txt
block (Lemma l c txt)       = thm "LEMMA" l c txt
block (Proposition l c txt) = thm "PROPOSITION" l c txt
block (Proof l c txt)       = thm "PROOF" l c txt
block (Definition l c txt)  = thm "DEFINITION" l c txt
block (Exercise l c txt)    = thm "EXERCISE" l c txt
block (Note l c txt)        = thm "NOTE" l c txt
block (Remark l c txt)      = thm "REMARK" l c txt
block (Problem l c txt)     = thm "PROBLEM" l c txt
block (Question l c txt)    = thm "QUESTION" l c txt
block (Solution l c txt)    = thm "SOLUTION" l c txt
block (Example l c txt)     = thm "EXAMPLE" l c txt

-- -------------------------------------------------------- [ Write List (String, String) ]

properties : List (String, String) -> String
properties Nil = ""
properties ps  = unlines
    [ macro "title"  $ fromMaybe "title missing"  (lookup "TITLE" ps)
    , macro "author" $ fromMaybe "author missing" (lookup "AUTHOR" ps)
    , macro "date"   $ fromMaybe "date missing"   (lookup "DATE" ps)
    , "\n"
    ]
  where
    ps' : List (String, String)
    ps' = nubAttribute "TITLE" $ nubAttribute "AUTHOR" $ nubAttribute "DATE" ps

-- --------------------------------------------------------------- [ Write Org ]

--@ TODO Add customisable preamble, and standalone
||| Convert document to LaTeX instance.
export
latex : Edda PRIME MODEL -> String
latex (MkEdda ps body) = unlines
    [ """\documentclass{article}
\usepackage{thmtools}
\usepackage[normelem]{ulem}
\usepackage{hyperref}

\declaretheoremstyle[%
  spaceabove=6pt, spacebelow=6pt,
  headfont=\normalfont\bfseries,
  bodyfont=\normalfont\em,
  postheadspace=1em
]{thmstyle}

\declaretheoremstyle[%
  spaceabove=6pt, spacebelow=6pt,
  headfont=\normalfont\bfseries,
  bodyfont=\normalfont,
  postheadspace=1em
]{notestyle}

\newcommand{\squote}[1]{`#1'}
\newcommand{\dquote}[1]{``#1''}

\declaretheorem[style=notestyle, starred, name={\textbf{Note}}]{note}
\declaretheorem[style=notestyle, starred, name={\textbf{Remark}}]{remark}
\declaretheorem[style=thmstyle, name={Definition}]{definition}
\declaretheorem[style=thmstyle, name={Example}]{example}
\declaretheorem[style=thmstyle, name={Exercise}]{exercise}
\declaretheorem[style=thmstyle, name={Problem}]{problem}
\declaretheorem[style=thmstyle, name={Question}]{question}
\declaretheorem[style=thmstyle, name={Solution}]{solution}

\declaretheorem[style=thmstyle, name={Corollary}]{corollary}
\declaretheorem[style=thmstyle, name={Lemma}]{lemma}
\declaretheorem[style=thmstyle, name={Proposition}]{proposition}
\declaretheorem[style=thmstyle, name={Theorem}]{theorem}
"""
    , properties ps
    , "\\begin{document}\n\\maketitle"
    , concatMap block body
    , "\\end{document}"
    ]


||| Write LaTeX representation to file.
export
writeLaTeX : String
         -> Edda PRIME MODEL
         -> Eff (Either String ()) [FILE_IO (), EXCEPTION String]
writeLaTeX fn doc = writeEddaFile latex fn doc

-- --------------------------------------------------------------------- [ EOF ]
