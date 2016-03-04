-- -------------------------------------------------------------- [ GapDoc.idr ]
-- Module    : GapDoc.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Edda.Writer.GapDoc

import Effects
import Effect.File
import Effect.Exception
import Effect.State

import Edda.Model
import Edda.Utils

import Edda.Writer.Common

-- -------------------------------------------------------------- [ Directives ]

%access private

-- ------------------------------------------------------------ [ Misc Writing ]

tag : String -> String -> String
tag n c = concat ["<",n,">",c,"</",n,">"]

-- ----------------------------------------------------------- [ Write Inlines ]

mutual
  inlines : List (Edda PRIME INLINE) -> String
  inlines xs = concatMap inline xs

  parens : String -> String -> List (Edda PRIME INLINE) -> String
  parens l r ts = concat [l, inlines ts, r]

  inline : Edda PRIME INLINE -> String
  inline (Text t) = t
  inline (Sans t) = t
  inline (Scap t) = t
  inline (Mono t) = t
  inline (Verb v)     = tag "C" v
  inline (Code v)     = tag "C" v
  inline (Math v)     = tag "M" v
  inline (Emph t)     = tag "E" (inlines t)
  inline (Bold t)     = tag "E" (inlines t)
  inline (Strike t)   = inlines t
  inline (Uline t)    = inlines t
  inline (Quote ty t) =
    case ty of
      SQuote => tag "Q" (inlines t)
      DQuote => tag "Q" (inlines t)
  inline (Parens ty t) =
    case ty of
      Parents  => parens "(" ")" t
      Brackets => parens "[" "]" t
      Braces   => parens "{" "}" t
  inline (Ref url)        = tag "URL" (tag "Link" url)
  inline (Hyper uri desc) = tag "URL" (with List concat [tag "Link" uri, tag "LinkText" (inlines desc)])
  inline (FNote l d)   = parens "(" ")" d
  inline (Cite ty uri) =
    case ty of
      ParenSty => concat ["<Cite Key=\"", uri, "\"/>"]
      TextSty  => concat ["<Cite Key=\"", uri, "\"/>"]
  inline (MiscPunc c) =
    case c of
      '%' => "%"
      '_' => "_"
      '^' => "^"
      '~' => "~"
      '&' => "&amp;"
      c   => cast c
  inline Space      = " "
  inline Newline    = "\n"
  inline Tab        = "\t"
  inline LBrace     = "{"
  inline RBrace     = "}"
  inline LParen     = "("
  inline RParen     = ")"
  inline LBrack     = "["
  inline RBrack     = "]"
  inline LAngle     = "&lt;"
  inline RAngle     = "&gt;"
  inline Dollar     = "$"
  inline Colon      = ":"
  inline Semi       = ";"
  inline EnDash     = "--"
  inline EmDash     = "---"
  inline FSlash     = "/"
  inline BSlash     = "\\"
  inline Apostrophe = "&apos;"
  inline SMark      = "&quot";
  inline Comma      = ","
  inline Plus       = "+"
  inline Ellipsis   = "..."
  inline Hyphen     = "-"
  inline Bang       = "!"
  inline Period     = "."
  inline QMark      = "?"
  inline Hash       = "#"
  inline Equals     = "="
  inline Pipe       = "|"

-- ----------------------------------------------------- [ Write Generic Block ]

-- Make generic
list : String -> List (List (Edda PRIME INLINE)) -> String
list s is = tag s (unlines $ map item is)
  where
    item : List (Edda PRIME INLINE) -> String
    item bs = tag "Item" (inlines bs) ++ "\n"

dlist : List (Pair (List (Edda PRIME INLINE)) (List (Edda PRIME INLINE))) -> String
dlist kvs = tag "Item" (unlines $ map descItem kvs)
  where
    descItem : (List (Edda PRIME INLINE), List (Edda PRIME INLINE)) -> String
    descItem (k,v) = tag "Item"
      (concat [ tag "E" (inlines k)
              , inlines v
              ]) ++ "\n"

secLvl : List (Edda PRIME INLINE) -> String
secLvl t = "<!-- Depth not recognised -->" ++ inlines t

-- ------------------------------------------------------------- [ Write Block ]
-- deal with attrs
block : Edda PRIME BLOCK -> String
block (HRule PRIME)                      = "<Hr/>"
block (Empty PRIME)                      = "\n"
block (Section PRIME lvl label title as) = unwords ["<!-- Depth Noth Recognised -->", inlines title]
block (Figure PRIME l c as fig)          = tag "E" "Figures not Supported"
block (DList PRIME kvs)                  = dlist kvs
block (OList bs)                         = list "Item"  bs
block (BList bs)                         = list "Enum" bs

block (Para txt) = inlines txt ++ "\n\n"

block (Listing l c lang langopts as src) = tag "Example" src

block (Comment ss)          = concat ["<!-- \n", ss, "\n-->"]
block (Equation l eq)       = tag "Display" eq
block (Literal l c src)     = tag "Verb" src

block (Quotation l txt)     = unwords [tag "E" "QUOTE",       inlines txt, "\n"]
block (Theorem l c txt)     = unwords [tag "E" "Theorem",     inlines txt, "\n"]
block (Corollary l c txt)   = unwords [tag "E" "COROLLARY",   inlines txt, "\n"]
block (Lemma l c txt)       = unwords [tag "E" "LEMMA",       inlines txt, "\n"]
block (Proposition l c txt) = unwords [tag "E" "PROPOSITION", inlines txt, "\n"]
block (Proof l c txt)       = unwords [tag "E" "PROOF",       inlines txt, "\n"]
block (Definition l c txt)  = unwords [tag "E" "DEFINITION",  inlines txt, "\n"]
block (Exercise l c txt)    = unwords [tag "E" "EXERCISE",    inlines txt, "\n"]
block (Note l c txt)        = unwords [tag "E" "NOTE",        inlines txt, "\n"]
block (Remark l c txt)      = unwords [tag "E" "REMARK",      inlines txt, "\n"]
block (Problem l c txt)     = unwords [tag "E" "PROBLEM",     inlines txt, "\n"]
block (Question l c txt)    = unwords [tag "E" "QUESTION",    inlines txt, "\n"]
block (Solution l c txt)    = unwords [tag "E" "SOLUTION",    inlines txt, "\n"]
block (Example l c txt)     = unwords [tag "E" "EXAMPLE",     inlines txt, "\n"]

export
blocks : String -> List (Edda PRIME BLOCK) -> String
blocks s Nil                     = s
blocks s (Section _ Z _ t _::xs) =
    unlines [ concat ["<Section Label=\", inlines t, \">"]
            , concat ["\t", tag "Heading" (inlines t)]
            , blocks s xs
            , "</Section>"]
blocks s (Section _  (S Z) _ t _::xs) =
    unlines [ concat ["<Subsection Label=\", inlines t, \">"]
            , concat ["\t", tag "Heading" (inlines t)]
            , blocks s xs
            , "</Subsection>"]

blocks s (x::xs) = blocks (unlines [s,block x]) xs

-- -------------------------------------------------------- [ Write List (String, String) ]

properties : List (String, String) -> String
properties Nil = ""
properties ps  = unlines
    [ "<TitlePage>"
    , tag "Title"  $ fromMaybe "title missing"  (lookup "TITLE" ps)
    , tag "Author" $ fromMaybe "author missing" (lookup "AUTHOR" ps)
    , "<Copyright></Copyright>"
    , "</Titlepage>"
    ]
  where
    ps' : List (String, String)
    ps' = nubAttribute "TITLE" $ nubAttribute "AUTHOR" $ nubAttribute "DATE" ps

-- --------------------------------------------------------------- [ Write Org ]

--@ TODO Add customisable preamble, and standalone
||| Convert document to LaTeX instance.
export
gapdoc : Edda PRIME MODEL -> String
gapdoc (MkEdda ps body) = unlines
    [ """<?xml version="1.0" encoding="UTF-8"?>
<Book>
"""
    , properties ps
    , "<TableOfContents/>"
    , "<Body>"
    , blocks "" body
    , "</Body>"
    , "</Book>"
    ]


||| Write LaTeX representation to file.
export
writeGapDoc : String
         -> Edda PRIME MODEL
         -> Eff (Either String ()) [FILE_IO (), EXCEPTION String]
writeGapDoc fn doc = writeEddaFile gapdoc fn doc

-- --------------------------------------------------------------------- [ EOF ]
