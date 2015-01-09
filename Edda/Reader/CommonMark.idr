module Edda.Reader.CommonMark

import public Control.Monad.Identity
import public Lightyear.Core
import public Lightyear.Combinators
import public Lightyear.Strings

import Edda.Effs
import Edda.Model
import Edda.Utils

import Edda.Reader.Utils
import Edda.Reader.Common

%access public

-- ------------------------------------------------------------------ [ Inline ]


code : Parser (Edda STAR INLINE)
code = map (Raw CodeTy) (literallyBetween '`') <?> "Code"

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
  txt <- angles' url
  pure $ Link ExposedTy txt Nil

hyper : Parser (Edda STAR INLINE)
hyper = do
  d <- brackets' $ some (text <$ space)
  uri  <- parens' $ url
  let desc = intersperse (Punc ' ') d
  pure $ Link HyperTy uri (desc)

link : Parser (Edda STAR INLINE)
link = hyper <|> expLink <?> "Links"

inline : Parser (Edda STAR INLINE)
inline = text
     <|> link
     <|> bold <|> emph
     <|> code
     <|> punc
     <?> "Raw Inline"


figure : Parser (Edda STAR BLOCK)
figure = do
    char '!'
    d <- brackets' $ some text
    let desc = intersperse (Punc ' ') d
    uri <- parens' url
    let img = Link ExposedTy uri Nil
    eol
    eol
    pure (Figure STAR "" desc Nil img)
  <?> "Figure"

-- ------------------------------------------------------------------- [ Lists ]
ulMarker : Parser ()
ulMarker = char '+' <|> char '-' <|> char '*' <?> "UList Marker"

olMarker : Parser ()
olMarker = marker '.' <|> marker ')'
  where
    marker : Char -> Parser ()
    marker c = do
      some $ satisfy (isDigit)
      char c
      (satisfy isSpace)
      pure ()

-- @TODO Add coninuations
listItem : Parser () -> Parser (List (Edda STAR INLINE))
listItem mark = do
    mark
    char ' '
    line <- manyTill inline eol
    pure $ line

olist : Parser (Edda STAR BLOCK)
olist = do
    is <- some (listItem olMarker)
    eol
    pure $ ListBlock NumberTy is

blist : Parser (Edda STAR BLOCK)
blist = do
    is <- some (listItem ulMarker)
    eol
    pure $ ListBlock BulletTy is

list : Parser (Edda STAR BLOCK)
list = blist <|> olist
-- ------------------------------------------------------------------ [ Blocks ]

indentedcode : Parser (Edda STAR BLOCK)
indentedcode = identcode "\t" <|> identcode "    " <?> "Indented Code Block"
  where
    identcode : String -> Parser (Edda STAR BLOCK)
    identcode m = do
      ss <- some $ (string m $!> manyTill (anyChar) eol)
      eol
      let src = concatMap (\x => pack (x ++ ['\n'])) ss
      pure $ VerbBlock LiteralTy Nothing Nil Nil src
     <?> "Indented Code"

fencedcode : Parser (Edda STAR BLOCK)
fencedcode = fencedcode' "```" <|> fencedcode' "~~~" <?> "Fenced Code Block"
  where
    fencedcode' : String -> Parser (Edda STAR BLOCK)
    fencedcode' m = do
        string m
        srcopts <- opt $ char ' ' $> manyTill (anyChar) eol
        let as = dealWithSrcAttrs (convertOpts srcopts) Nil
        src <- manyTill anyChar (string m)
        eol
        pure $ VerbBlock ListingTy Nothing Nil as (pack src)
      <?> "Fenced Code Block: " ++ m

blockquote : Parser (Edda STAR BLOCK)
blockquote = do
    txt <- some $ (token ">" $!> manyTill inline eol)
    eol
    let p = concat txt
    pure $ TextBlock QuotationTy Nothing Nil Nil p

-- ------------------------------------------------------------------- [ Paras ]
para : Parser (Edda STAR BLOCK)
para = do
    txt <- manyTill (inline) (eol $> eol)
    pure $ TextBlock ParaTy Nothing Nil Nil txt
  <?> "Paragraphs"

paraLast : Parser (Edda STAR BLOCK)
paraLast = do
    txt <- manyTill inline (eol $> space)
    pure $ TextBlock ParaTy Nothing Nil Nil txt
  <?> "Filthy hack for last para"

hrule : Parser (Edda STAR BLOCK)
hrule = hrule' "***" <|> hrule' "---" <|> hrule' "___" <?> "hrules"
  where
    hrule' m = do
      string m
      eol
      pure $ HRule STAR

empty : Parser (Edda STAR BLOCK)
empty = do
  eol
  eol
  pure $ Empty STAR

mutual
  block : Nat -> Parser (Edda STAR BLOCK)
  block lvl = header lvl
      <|> blockquote <|> indentedcode <|> fencedcode
      <|> list <|> figure <|> hrule <|> para <|> empty
      <?> "Block"

  header : Nat -> Parser (Edda STAR BLOCK)
  header lvl = do
      char '#'
      depth <- opt (many $ char '#')
      let d = length (fromMaybe Nil depth) + 1
      if not (d <= lvl)
        then fail "a"
        else do
           space
           title <- manyTill (inline) (eol)
           eol
           ds <- some (block (S lvl))
           pure (Section STAR d Nothing title ds)
    <?> "Header"



parseCommonMark : Parser (Edda STAR MODEL)
parseCommonMark = do
    txt <- some $ header Z
    let txt' = intersperse (Empty STAR) txt
    pure $ EddaRaw Nil (txt' ++ [Empty STAR])
  <?> "Raw Common Mark"

-- -------------------------------------------------------------------- [ Read ]
public
readCommonMark : String -> {[FILE_IO ()]} Eff (Either String (Edda PRIME MODEL))
readCommonMark = readEddaFile parseCommonMark
