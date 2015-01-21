module Edda.Writer

import public Effects
import public Effect.File
import public Effect.Exception

import Edda.Model
import Edda.Utils

import Edda.Writer.Common

%access private

-- ------------------------------------------------------------ [ Misc Writing ]
writeRawTag : String -> String -> {[FILE_IO (OpenFile Write)]} Eff ()
writeRawTag key value = do
    writeString "#+"
    writeString key
    writeString ": "
    writeLine value

writeThing : Char -> Nat -> {[FILE_IO (OpenFile Write)]} Eff ()
writeThing c Z = pure ()
writeThing c (S k) = do
    writeString (cast c)
    writeThing c k

writeAttrs : List (String, String) -> {[FILE_IO (OpenFile Write)]} Eff ()
writeAttrs Nil = pure ()
writeAttrs as  = do
    writeRawTag "ATTR" $ unwords $ map (\(k,v) => k ++ ":" ++ v) as
    writeLine ""

-- ----------------------------------------------------------- [ Write Inlines ]
mutual
  writeTag : String
           -> List (Edda PRIME INLINE)
           -> {[FILE_IO (OpenFile Write)]} Eff ()
  writeTag _   Nil   = pure ()
  writeTag key value = do
     writeString "#+"
     writeString key
     writeString ": "
     writeInlines value
     writeString "\n"

  writeParens : Char -> Char -> Either String (List (Edda PRIME INLINE))
              -> {[FILE_IO (OpenFile Write)]} Eff ()
  writeParens l r txt = do
      writeString (cast l)
      case txt of
        Left str  => writeString str
        Right txt => writeInlines txt
      writeString (cast r)

  writeMarkup : Char -> Either String (List (Edda PRIME INLINE))
              -> {[FILE_IO (OpenFile Write)]} Eff ()
  writeMarkup t txt = writeParens t t txt

  writeLink : String
            -> (List (Edda PRIME INLINE))
            -> {[FILE_IO (OpenFile Write)]} Eff ()
  writeLink uri Nil = do
      writeString "[["
      writeString uri
      writeString "]]"
  writeLink uri desc = do
      writeString "["
      writeString "["
      writeString uri
      writeString "]"
      writeString "["
      writeInlines desc
      writeString "]"
      writeString "]"

  writeInline : Edda PRIME INLINE -> {[FILE_IO (OpenFile Write)]} Eff ()
  writeInline (Text t) = writeString t
  writeInline (Sans t) = writeString t
  writeInline (Scap t) = writeString t
  writeInline (Mono t) = writeString t
  writeInline (Verb v) = writeMarkup '=' (Left v)
  writeInline (Code v) = writeMarkup '~' (Left v)
  writeInline (Math v) = writeMarkup '$' (Left v)
  writeInline (Emph t) = writeMarkup '/' (Right t)
  writeInline (Bold t) = writeMarkup '*' (Right t)
  writeInline (Strike t) = writeMarkup '+' (Right t)
  writeInline (Uline t) = writeMarkup '_' (Right t)
  writeInline (Quote ty t) = case ty of
    SQuote => writeMarkup '\'' (Right t)
    DQuote => writeMarkup '\"' (Right t)
  writeInline (Parens ty t) = case ty of
    Parents  => writeParens '(' ')' (Right t)
    Brackets => writeParens '[' ']' (Right t)
    Braces   => writeParens '{' '}' (Right t)
  writeInline (Ref url) = writeLink url Nil
  writeInline (Hyper uri desc) = writeLink uri desc
  writeInline (FNote l d) = case d of
    Nil => writeParens '[' ']' (Left ("fn" ++ ":" ++ l ++ ":"))
    x   => writeParens '[' ']' (Right ([Text ("fn:" ++ l ++ ":")] ++ x))
  writeInline (Cite ty uri) = case ty of
    ParenSty => writeString ("[[citep:" ++ uri ++ "]]")
    TextSty  => writeString ("[[citet:" ++ uri ++ "]]")
  writeInline (MiscPunc c) = writeString (cast c)
  writeInline Space      = writeString " "
  writeInline Newline    = writeString "\n"
  writeInline Tab        = writeString "\t"
  writeInline LBrace     = writeString "{"
  writeInline RBrace     = writeString "}"
  writeInline LParen     = writeString "("
  writeInline RParen     = writeString ")"
  writeInline LBrack     = writeString "["
  writeInline RBrack     = writeString "]"
  writeInline LAngle     = writeString "<"
  writeInline RAngle     = writeString ">"
  writeInline Dollar     = writeString "$"
  writeInline Colon      = writeString ":"
  writeInline Semi       = writeString ";"
  writeInline EnDash     = writeString "--"
  writeInline EmDash     = writeString "---"
  writeInline FSlash     = writeString "/"
  writeInline BSlash     = writeString "\\"
  writeInline Apostrophe = writeString "'"
  writeInline SMark      = writeString "\""
  writeInline Comma      = writeString ","
  writeInline Plus       = writeString "+"
  writeInline Ellipsis   = writeString "..."
  writeInline Hyphen     = writeString "-"
  writeInline Bang       = writeString "!"
  writeInline Period     = writeString "."
  writeInline QMark      = writeString "?"
  writeInline Hash       = writeString "#"
  writeInline Equals     = writeString "="
  writeInline Pipe       = writeString "|"

-- ----------------------------------------------------------- [ Write Inlines ]
  writeInlines : List (Edda PRIME INLINE) -> {[FILE_IO (OpenFile Write)]} Eff ()
  writeInlines = writeManyThings (writeInline)
-- ----------------------------------------------------- [ Write Generic Block ]

writeGenBlock : (a -> {[FILE_IO (OpenFile Write)]} Eff ())
              -> String
              -> Maybe String
              -> List (Edda PRIME INLINE)
              -> a
              -> {[FILE_IO (OpenFile Write)]} Eff ()
writeGenBlock f tag l c b = do
    writeTag "CAPTION" c
    writeMaybe (\x => writeRawTag "NAME" x) l
    writeLine ("#+BEGIN_" ++ tag)
    f b
    writeLine ("#+END_" ++ tag)
    writeString "\n"

writeTextBlock : String
              -> Maybe String
              -> List (Edda PRIME INLINE)
              -> List (Edda PRIME INLINE)
              -> {[FILE_IO (OpenFile Write)]} Eff ()
writeTextBlock = writeGenBlock (writeInlines)

writeVerbBlock : String
              -> Maybe String
              -> List (Edda PRIME INLINE)
              -> String
              -> {[FILE_IO (OpenFile Write)]} Eff ()
writeVerbBlock = writeGenBlock (writeString)

-- ------------------------------------------------------------- [ Write Block ]
writeDefItem : (List (Edda PRIME INLINE), List (Edda PRIME INLINE)) -> {[FILE_IO (OpenFile Write)]} Eff ()
writeDefItem (k, vs) = do
    writeString "- "
    writeInlines k
    writeString " :: "
    writeInlines vs
    writeString "\n"

writeItem : String -> List (Edda PRIME INLINE) -> {[FILE_IO (OpenFile Write)]} Eff ()
writeItem mark b = do
    writeString mark
    writeInlines b
    writeString "\n"

mutual
  writeBlock : Edda PRIME BLOCK -> {[FILE_IO (OpenFile Write)]} Eff ()
  writeBlock (HRule PRIME) = writeString "-----"
  writeBlock (Empty PRIME) = writeString ""
  writeBlock (Section PRIME lvl label title as) = do
      writeThing '*' lvl
      writeString " "
      writeInlines title
      writeString "\n"
      writeString $ fromMaybe "" label
      writeString "\n"
  writeBlock (Figure PRIME l c as fig) = do
      writeTag "CAPTION" c
      writeRawTag "NAME" l
      writeAttrs as
      writeInline fig
      writeString "\n\n"
  writeBlock (DList PRIME kvs) = do
      writeManyThings writeDefItem kvs
      writeString "\n"
  writeBlock (OList bs) = do
      writeManyThings (writeItem "1. ") bs
      writeString "\n"
  writeBlock (BList bs) = do
      writeManyThings (writeItem "+ ") bs
      writeString "\n"
  writeBlock (Para txt) = do
      writeInlines txt
      writeLine "\n"
  writeBlock (Listing l c lang langopts as src) = do
      writeTag "CAPTION" c
      writeRawTag "NAME" $ fromMaybe "MISSING" l
      writeAttrs as
      writeString "#+BEGIN_SRC"
      writeString " "
      writeString (fromMaybe "" lang)
      writeString " "
      writeLine (fromMaybe "" langopts)
      writeString src
      writeLine "#+END_SRC"
      writeString "\n"
  writeBlock (Comment ss)          = writeVerbBlock "COMMENT" Nothing Nil ss
  writeBlock (Equation l eq)       = writeVerbBlock "EQUATION" l Nil eq
  writeBlock (Literal l c src)     = writeVerbBlock "EXAMPLE" l c src
  writeBlock (Quotation l txt)     = writeTextBlock "QUOTE" l Nil txt
  writeBlock (Theorem l c txt)     = writeTextBlock "Theorem" l c txt
  writeBlock (Corollary l c txt)   = writeTextBlock "COROLLARY" l c txt
  writeBlock (Lemma l c txt)       = writeTextBlock "LEMMA" l c txt
  writeBlock (Proposition l c txt) = writeTextBlock "PROPOSITION" l c txt
  writeBlock (Proof l c txt)       = writeTextBlock "PROOF" l c txt
  writeBlock (Definition l c txt)  = writeTextBlock "DEFINITION" l c txt
  writeBlock (Exercise l c txt)    = writeTextBlock "EXERCISE" l c txt
  writeBlock (Note l c txt)        = writeTextBlock "NOTE" l c txt
  writeBlock (Remark l c txt)      = writeTextBlock "REMARK" l c txt
  writeBlock (Problem l c txt)     = writeTextBlock "PROBLEM" l c txt
  writeBlock (Question l c txt)    = writeTextBlock "QUESTION" l c txt
  writeBlock (Solution l c txt)    = writeTextBlock "SOLUTION" l c txt
  writeBlock (Example l c txt)     = writeTextBlock "EXAMPLE" l c txt

  -- -------------------------------------------------------------- [ Write Body ]
  writeBlocks : List (Edda PRIME BLOCK) -> {[FILE_IO (OpenFile Write)]} Eff ()
  writeBlocks = writeManyThings (writeBlock)

-- -------------------------------------------------------- [ Write List (String, String) ]
writeProps : List (String, String) -> {[FILE_IO (OpenFile Write)]} Eff ()
writeProps Nil = pure ()
writeProps ps = do
    writeRawTag "TITLE" title
    writeRawTag "AUTHOR" author
    writeRawTag "DATE" date
    let ps' = nubAttribute "TITLE" $ nubAttribute "AUTHOR" $ nubAttribute "DATE" ps
    writeManyThings (\(k,v) => writeRawTag k v) ps'
    writeString "\n"
  where
    title = fromMaybe "title missing" (lookup "TITLE" ps)
    author = fromMaybe "author missing" (lookup "AUTHOR" ps)
    date = fromMaybe "date missing" (lookup "DATE" ps)

-- --------------------------------------------------------------- [ Write Org ]
doWrite' : List (String, String)
         -> List (Edda PRIME BLOCK)
         -> {[FILE_IO (OpenFile Write)]} Eff ()
doWrite' ps body = do
    writeProps ps
    writeBlocks body

covering
doWrite : Edda PRIME MODEL -> {[FILE_IO (OpenFile Write)]} Eff ()
doWrite (MkEdda ps body) = doWrite' ps body

public
writeOrg : String
         -> Edda PRIME MODEL
         -> {[FILE_IO (), EXCEPTION String]} Eff ()
writeOrg = writeEddaFile (doWrite)
