-- ----------------------------------------------------------------- [ Org.idr ]
-- Module    : Org.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Edda.Writer.CommonMark

import Effects
import Effect.File
import Effect.Exception

import Edda.Model
import Edda.Utils

import Edda.Writer.Common

%access private

-- TODO Attributes...

-- ------------------------------------------------------------ [ Misc Writing ]
ntimes : Char -> Nat -> String
ntimes c n = concat $ ntimes' n
  where
    ntimes' : Nat -> List String
    ntimes' Z     = Nil
    ntimes' (S k) = (cast c) :: ntimes' k

-- ----------------------------------------------------------- [ Write Inlines ]

mutual
  inlines : List (Edda PRIME INLINE) -> String
  inlines xs = unwords $ map inline xs

  parens : String -> String -> Either String (List (Edda PRIME INLINE)) -> String
  parens l r (Left str) = concat [l, str,        r]
  parens l r (Right ts) = concat [l, inlines ts, r]

  markup : String -> Either String (List (Edda PRIME INLINE)) -> String
  markup t txt = parens t t txt

  link : String -> List (Edda PRIME INLINE) -> String
  link uri Nil  = concat ["<", uri, ">"]
  link uri desc = concat ["[", uri, "](", inlines desc, ")"]

  inline : Edda PRIME INLINE -> String
  inline (Text t)     = t
  inline (Sans t)     = t
  inline (Scap t)     = t
  inline (Mono t)     = t
  inline (Verb v)     = markup "`"  (Left v)
  inline (Code v)     = markup "`"  (Left v)
  inline (Math v)     = markup "`"  (Left v)
  inline (Emph t)     = markup "*"  (Right t)
  inline (Bold t)     = markup "**" (Right t)
  inline (Strike t)   = markup "~~" (Right t)
  inline (Uline t)    = markup "*"  (Right t)
  inline (Quote ty t) =
    case ty of
      SQuote => markup "'"  (Right t)
      DQuote => markup "\"" (Right t)
  inline (Parens ty t) =
    case ty of
      Parents  => parens "(" ")" (Right t)
      Brackets => parens "[" "]" (Right t)
      Braces   => parens "{" "}" (Right t)
  inline (Ref url)        = link url Nil
  inline (Hyper uri desc) = link uri desc
  inline (FNote l d) =
    case d of
      Nil => parens "`" "`" (Left (concat ["fn", ":", l, ":"]))
      x   => parens "`" "`" (Right ([Text ("fn:" ++ l ++ ":")] ++ x))
  inline (Cite ty uri) =
    case ty of
      ParenSty => ("`citep:" ++ uri ++ "`")
      TextSty  => ("`citet:" ++ uri ++ "`")
  inline (MiscPunc c) = (cast c)
  inline Space      = " "
  inline Newline    = "\n"
  inline Tab        = "\t"
  inline LBrace     = "{"
  inline RBrace     = "}"
  inline LParen     = "("
  inline RParen     = ")"
  inline LBrack     = "["
  inline RBrack     = "]"
  inline LAngle     = "<"
  inline RAngle     = ">"
  inline Dollar     = "$"
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
  inline Ellipsis   = "..."
  inline Hyphen     = "-"
  inline Bang       = "!"
  inline Period     = "."
  inline QMark      = "?"
  inline Hash       = "#"
  inline Equals     = "="
  inline Pipe       = "|"

-- ----------------------------------------------------- [ Write Generic Block ]

genblock : (a -> String)
        -> a
        -> String
genblock f b = unlines [ f b, "\n"]

textblock : List (Edda PRIME INLINE)
         -> String
textblock = genblock (inlines)

verbblock : String
         -> String
verbblock = genblock (\x => ">" ++ x)

-- ------------------------------------------------------------- [ Write Block ]

itemDef : (List (Edda PRIME INLINE), List (Edda PRIME INLINE)) -> String
itemDef (k,vs) =
    unwords ["+", markup "*" (Left $ inlines k), "::", inlines vs, "\n"]

item : String -> List (Edda PRIME INLINE) -> String
item m b = unwords [m, inlines b, "\n"]

public
block : Edda PRIME BLOCK -> String
block (HRule PRIME) = "-----"
block (Empty PRIME) = ""
block (Section PRIME lvl label title as) =
    unlines [ntimes '#' lvl, inlines title, fromMaybe "" label, "\n"]

block (Figure PRIME l c as fig) = unlines [inline fig, "\n"]

block (DList PRIME kvs) = (unlines $ map itemDef kvs)    ++ "\n"
block (OList bs)        = (unlines $ map (item "1.") bs) ++ "\n"
block (BList bs)        = (unlines $ map (item "*")  bs) ++ "\n"
block (Para txt)        = inlines txt ++ "\n"

block (Listing l c lang langopts as src) =
    unlines [ "\n```" ++ fromMaybe "" lang , src, "```\n"]

block (Comment ss)          = verbblock ss
block (Equation l eq)       = verbblock eq
block (Literal l c src)     = verbblock src

block (Quotation l txt)     = textblock txt

block (Theorem l c txt)     = textblock txt
block (Corollary l c txt)   = textblock txt
block (Lemma l c txt)       = textblock txt
block (Proposition l c txt) = textblock txt
block (Proof l c txt)       = textblock txt
block (Definition l c txt)  = textblock txt
block (Exercise l c txt)    = textblock txt
block (Note l c txt)        = textblock txt
block (Remark l c txt)      = textblock txt
block (Problem l c txt)     = textblock txt
block (Question l c txt)    = textblock txt
block (Solution l c txt)    = textblock txt
block (Example l c txt)     = textblock txt

-- -------------------------------------------------------- [ Write List (String, String) ]

properties : List (String, String) -> String
properties Nil = ""
properties ps  =
  unlines [ "%YAML 1.2\n---"
          , concat ["title:",  fromMaybe "title missing" (lookup "TITLE" ps)]
          , concat ["author:", fromMaybe "author missing" (lookup "AUTHOR" ps)]
          , concat ["date:",   fromMaybe "date missing" (lookup "DATE" ps)]
          , "...\n"
          ]

-- --------------------------------------------------------------- [ Write Org ]
public
markdown : Edda PRIME MODEL -> String
markdown (MkEdda ps body) = unlines $ (properties ps :: map block body)

public
writeMarkdown : String
         -> Edda PRIME MODEL
         -> Eff () [FILE_IO (), EXCEPTION String]
writeMarkdown fn doc = writeEddaFile markdown fn doc

-- --------------------------------------------------------------------- [ EOF ]
