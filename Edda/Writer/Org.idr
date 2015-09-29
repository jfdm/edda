-- ----------------------------------------------------------------- [ Org.idr ]
-- Module    : Org.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Edda.Writer.Org

import Effects
import Effect.File
import Effect.Exception

import Edda.Model
import Edda.Utils

import Edda.Writer.Common

%access private

-- ------------------------------------------------------------ [ Misc Writing ]
rawtag : String -> String -> String
rawtag k v = unwords ["#+" ++ k ++ ":", v]

ntimes : Char -> Nat -> String
ntimes c n = concat $ ntimes' n
  where
    ntimes' : Nat -> List String
    ntimes' Z     = Nil
    ntimes' (S k) = (cast c) :: ntimes' k

attrs : List (String, String) -> String
attrs as = rawtag "ATTR" as'
  where
    as' : String
    as' = unwords $ map (\(k,v) => k ++ ":" ++ v) as

-- ----------------------------------------------------------- [ Write Inlines ]

mutual
  public
  inlines : List (Edda PRIME INLINE) -> String
  inlines xs = concatMap inline xs

  tag : String -> List (Edda PRIME INLINE) -> String
  tag k vs = unlines [rawtag k (inlines vs), "\n"]

  parens : Char -> Char -> Either String (List (Edda PRIME INLINE)) -> String
  parens l r (Left str) = concat [cast l, str,        cast r]
  parens l r (Right ts) = concat [cast l, inlines ts, cast r]

  markup : Char -> Either String (List (Edda PRIME INLINE)) -> String
  markup t txt = parens t t txt

  link : String -> List (Edda PRIME INLINE) -> String
  link uri Nil  = concat ["[[", uri, "]]"]
  link uri desc = concat ["[[", uri, "][", inlines desc, "]]"]

  inline : Edda PRIME INLINE -> String
  inline (Text t) = t
  inline (Sans t) = t
  inline (Scap t) = t
  inline (Mono t) = t
  inline (Verb v)     = markup '=' (Left v)
  inline (Code v)     = markup '~' (Left v)
  inline (Math v)     = markup '$' (Left v)
  inline (Emph t)     = markup '/' (Right t)
  inline (Bold t)     = markup '*' (Right t)
  inline (Strike t)   = markup '+' (Right t)
  inline (Uline t)    = markup '_' (Right t)
  inline (Quote ty t) =
    case ty of
      SQuote => markup '\'' (Right t)
      DQuote => markup '\"' (Right t)
  inline (Parens ty t) =
    case ty of
      Parents  => parens '(' ')' (Right t)
      Brackets => parens '[' ']' (Right t)
      Braces   => parens '{' '}' (Right t)
  inline (Ref url)        = link url Nil
  inline (Hyper uri desc) = link uri desc
  inline (FNote l d) =
    case d of
      Nil => parens '[' ']' (Left (concat ["fn", ":", l, ":"]))
      x   => parens '[' ']' (Right ([Text ("fn:" ++ l ++ ":")] ++ x))
  inline (Cite ty uri) =
    case ty of
      ParenSty => ("[[citep:" ++ uri ++ "]]")
      TextSty  => ("[[citet:" ++ uri ++ "]]")
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
        -> String
        -> Maybe String
        -> List (Edda PRIME INLINE)
        -> a
        -> String
genblock f t l c b = unlines
  [ tag "CAPTION" c
  , strFromMaybe (\x => rawtag "NAME" x) l
  , "#+BEGIN_" ++ t
  , f b
  , "#+END_" ++ t
  , "\n"
  ]

textblock : String
         -> Maybe String
         -> List (Edda PRIME INLINE)
         -> List (Edda PRIME INLINE)
         -> String
textblock = genblock (inlines)

verbblock : String
         -> Maybe String
         -> List (Edda PRIME INLINE)
         -> String
         -> String
verbblock = genblock (\x => x)

-- ------------------------------------------------------------- [ Write Block ]

itemDef : (List (Edda PRIME INLINE), List (Edda PRIME INLINE)) -> String
itemDef (k,vs) = unwords ["-", inlines k, "::", inlines vs, "\n"]

item : String -> List (Edda PRIME INLINE) -> String
item m b = unwords [m, inlines b, "\n"]

public
block : Edda PRIME BLOCK -> String
block (HRule PRIME) = "-----"
block (Empty PRIME) = ""
block (Section PRIME lvl label title as) =
    unwords [ ntimes '*' lvl
            , inlines title
            , fromMaybe "" label
            , "\n"]
block (Figure PRIME l c as fig) =
    unlines [ tag "CAPTION" c
            , rawtag "NAME" l
            , attrs as
            , inline fig
            , "\n"]
block (DList PRIME kvs) = (unlines $ map itemDef kvs)    ++ "\n"
block (OList bs)        = (unlines $ map (item "1.") bs) ++ "\n"
block (BList bs)        = (unlines $ map (item "+")  bs) ++ "\n"
block (Para txt) = inlines txt ++ "\n"
block (Listing l c lang langopts as src) =
    unlines [ tag "CAPTION" c
            , rawtag "NAME" $ fromMaybe "MISSING" l
            , attrs as
            , unwords ["#+BEGIN_SRC", fromMaybe "" lang, fromMaybe "" langopts]
            , src
            , "#+END_SRC\n"
            ]
block (Comment ss)          = verbblock "COMMENT" Nothing Nil ss
block (Equation l eq)       = verbblock "EQUATION" l Nil eq
block (Literal l c src)     = verbblock "EXAMPLE" l c src
block (Quotation l txt)     = textblock "QUOTE" l Nil txt
block (Theorem l c txt)     = textblock "Theorem" l c txt
block (Corollary l c txt)   = textblock "COROLLARY" l c txt
block (Lemma l c txt)       = textblock "LEMMA" l c txt
block (Proposition l c txt) = textblock "PROPOSITION" l c txt
block (Proof l c txt)       = textblock "PROOF" l c txt
block (Definition l c txt)  = textblock "DEFINITION" l c txt
block (Exercise l c txt)    = textblock "EXERCISE" l c txt
block (Note l c txt)        = textblock "NOTE" l c txt
block (Remark l c txt)      = textblock "REMARK" l c txt
block (Problem l c txt)     = textblock "PROBLEM" l c txt
block (Question l c txt)    = textblock "QUESTION" l c txt
block (Solution l c txt)    = textblock "SOLUTION" l c txt
block (Example l c txt)     = textblock "EXAMPLE" l c txt

-- -------------------------------------------------------- [ Write List (String, String) ]

properties : List (String, String) -> String
properties Nil = ""
properties ps  = unlines ts
  where
    ps' : List (String, String)
    ps' = nubAttribute "TITLE" $ nubAttribute "AUTHOR" $ nubAttribute "DATE" ps

    ts : List String
    ts = [ rawtag "TITLE"  $ fromMaybe "title missing" (lookup "TITLE" ps)
         , rawtag "AUTHOR" $ fromMaybe "author missing" (lookup "AUTHOR" ps)
         , rawtag "DATE"   $ fromMaybe "date missing" (lookup "DATE" ps)
         ]
         ++
         map (\(k,v) => rawtag k v) ps'
         ++
         ["\n"]

-- --------------------------------------------------------------- [ Write Org ]
public
org : Edda PRIME MODEL -> String
org (MkEdda ps body) = unlines $ (properties ps :: map block body)

public
writeOrg : String
         -> Edda PRIME MODEL
         -> Eff () [FILE_IO (), EXCEPTION String]
writeOrg fn doc = writeEddaFile org fn doc

-- --------------------------------------------------------------------- [ EOF ]
