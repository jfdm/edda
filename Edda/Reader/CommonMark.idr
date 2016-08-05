-- ---------------------------------------------------------- [ CommonMark.idr ]
-- Module    : CommonMark.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Edda.Reader.CommonMark

-- TODO Make recursive parsing.
import Effects
import Effect.File

import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Edda.Model
import Edda.Utils

import Edda.Reader.Utils
import public Edda.Reader.Common

%access private

-- ------------------------------------------------------------------ [ Inline ]

code : Parser (Edda STAR INLINE)
code = map (Raw CodeTy) (quoted '`') <?> "Code"

markup : MarkupTy -> String -> Parser (Edda STAR INLINE)
markup mTy c = do
    txt <- between (string c) (string c) (some mText)
    pure $ Mark mTy txt
  <?> "Markup"

bold : Parser (Edda STAR INLINE)
bold = markup BoldTy "**" <|> markup BoldTy "__" <?> "Bold"

emph : Parser (Edda STAR INLINE)
emph = markup EmphTy "*" <|> markup EmphTy "_" <?> "Emph"

expLink : Parser (Edda STAR INLINE)
expLink = do
  txt <- angles url
  pure $ Link ExposedTy txt Nil

hyper : Parser (Edda STAR INLINE)
hyper = do
  d <- brackets $ some (text <* spaces)
  uri  <- parens $ url
  let desc = intersperse (Punc ' ') d
  pure $ Link HyperTy uri (desc)

link : Parser (Edda STAR INLINE)
link = hyper <|> expLink <?> "Links"

inline : Parser (Edda STAR INLINE)
inline = text
     <|> link
     <|> bold
     <|> emph
     <|> code
     <|> punc
     <?> "Raw Inline"

figure : Parser (Edda STAR BLOCK)
figure = do
    char '!'
    d <- brackets $ some text
    let desc = intersperse (Punc ' ') d
    uri <- parens url
    let img = Link ExposedTy uri Nil
    endOfLine
    endOfLine
    pure (Figure STAR "" desc Nil img)
  <?> "Figure"

-- ------------------------------------------------------------------- [ Lists ]
ulMarker : Parser ()
ulMarker = char' '+' <|> char' '-' <|> char' '*' <?> "UList Marker"

olMarker : Parser ()
olMarker = marker '.' <|> marker ')'
  where
    marker : Char -> Parser ()
    marker c = do
      some $ digit
      char c
      (satisfy isSpace)
      pure ()

-- @TODO Add coninuations
listItem : Parser () -> Parser (List (Edda STAR INLINE))
listItem mark = do
    mark
    space
    line <- manyTill inline endOfLine
    pure $ line

olist : Parser (Edda STAR BLOCK)
olist = do
    is <- some (listItem olMarker)
    endOfLine
    pure $ ListBlock NumberTy is

blist : Parser (Edda STAR BLOCK)
blist = do
    is <- some (listItem ulMarker)
    endOfLine
    pure $ ListBlock BulletTy is

list : Parser (Edda STAR BLOCK)
list = blist <|> olist

-- ------------------------------------------------------------------ [ Blocks ]

indentedcode : Parser (Edda STAR BLOCK)
indentedcode = identcode "\t" <|> identcode "    " <?> "Indented Code Block"
  where
    identcode : String -> Parser (Edda STAR BLOCK)
    identcode m = do
      ss <- some $ (string m *!> manyTill (anyChar) endOfLine)
      endOfLine
      let src = concatMap (\x => pack (List.(++) x ['\n'])) ss
      pure $ VerbBlock LiteralTy Nothing Nil Nil src
     <?> "Indented Code"

fencedcode : Parser (Edda STAR BLOCK)
fencedcode = fencedcode' "```" <|> fencedcode' "~~~" <?> "Fenced Code Block"
  where
    fencedcode' : String -> Parser (Edda STAR BLOCK)
    fencedcode' m = do
        string m
        srcopts <- opt $ space *> manyTill (anyChar) endOfLine
        let as = dealWithSrcAttrs (convertOpts srcopts) Nil
        src <- manyTill anyChar (string m)
        endOfLine
        pure $ VerbBlock ListingTy Nothing Nil as (pack src)
      <?> "Fenced Code Block: " ++ m

blockquote : Parser (Edda STAR BLOCK)
blockquote = do
    txt <- some $ (token ">" *!> manyTill inline endOfLine)
    endOfLine
    let p = concat txt
    pure $ TextBlock QuotationTy Nothing Nil Nil p

-- ------------------------------------------------------------------- [ Paras ]
para : Parser (Edda STAR BLOCK)
para = do
    txt <- manyTill (inline) (endOfLine *> endOfLine)
    pure $ TextBlock ParaTy Nothing Nil Nil txt
  <?> "Paragraphs"

paraLast : Parser (Edda STAR BLOCK)
paraLast = do
    txt <- manyTill inline (endOfLine *> spaces)
    pure $ TextBlock ParaTy Nothing Nil Nil txt
  <?> "Filthy hack for last para"

hrule : Parser (Edda STAR BLOCK)
hrule = hrule' "***" <|> hrule' "---" <|> hrule' "___" <?> "hrules"
  where
    hrule' m = do
      string m
      endOfLine
      pure $ HRule STAR

empty : Parser (Edda STAR BLOCK)
empty = do
  endOfLine
  endOfLine
  pure $ Empty STAR

header : Parser (Edda STAR BLOCK)
header = char '#' >! do
    depth <- opt (many $ char '#')
    spaces
    title <- manyTill (inline) (endOfLine)
    endOfLine
    let d = length (fromMaybe Nil depth) + 1
    pure (Section STAR d Nothing title Nil)
  <?> "Header"

block : Parser (Edda STAR BLOCK)
block = header
    <|> blockquote <|> indentedcode <|> fencedcode
    <|> list <|> figure <|> hrule <|> para <|> empty
    <?> "Block"

parseCommonMark : Parser (Edda STAR MODEL)
parseCommonMark = do
    txt <- some block
    let txt' = intersperse (Empty STAR) txt
    pure $ EddaRaw Nil (txt' ++ [Empty STAR])
  <?> "Raw Common Mark"

-- -------------------------------------------------------------------- [ Read ]
export
readCommonMark : String -> Eff (Either String (Edda PRIME MODEL)) [FILE ()]
readCommonMark = readEddaFile parseCommonMark

-- --------------------------------------------------------------------- [ EOF ]
