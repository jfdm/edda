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


code : Parser (Inline Star)
code = map (Raw CodeTy) (literallyBetween '`') <?> "Code"

markup : MarkupTy -> String -> Parser (Inline Star)
markup mTy c = do
    txt <- between (string c) (string c) (some mText)
    pure $ Mark mTy txt
  <?> "Markup"

bold : Parser (Inline Star)
bold = markup BoldTy "**" <|> markup BoldTy "__" <?> "Bold"

emph : Parser (Inline Star)
emph = markup EmphTy "*" <|> markup EmphTy "_" <?> "Emph"

expLink : Parser (Inline Star)
expLink = do
  txt <- angles' url
  pure $ Link ExposedTy txt Nothing

hyper : Parser (Inline Star)
hyper = do
  d <- brackets' $ some (text <$ space)
  uri  <- parens' $ url
  let desc = intersperse (Punc ' ') d
  pure $ Link HyperTy uri (Just desc)

link : Parser (Inline Star)
link = hyper <|> expLink <?> "Links"

inline : Parser (Inline Star)
inline = text
     <|> link
     <|> bold <|> emph
     <|> code
     <|> punc
     <?> "Raw Inline"


figure : Parser (Block Star)
figure = do
    char '!'
    d <- brackets' $ some text
    let desc = intersperse (Punc ' ') d
    uri <- parens' url
    let img = Link ExposedTy uri Nothing
    eol
    eol
    pure (Figure Star "" desc Nothing img)
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
listItem : Parser () -> Parser (List (Inline Star))
listItem mark = do
    mark
    char ' '
    line <- manyTill inline eol
    pure $ line

olist : Parser (Block Star)
olist = do
    is <- some (listItem olMarker)
    eol
    pure $ ListBlock NumberTy is

blist : Parser (Block Star)
blist = do
    is <- some (listItem ulMarker)
    eol
    pure $ ListBlock BulletTy is

list : Parser (Block Star)
list = blist <|> olist
-- ------------------------------------------------------------------ [ Blocks ]

indentedcode : Parser (Block Star)
indentedcode = identcode "\t" <|> identcode "    " <?> "Indented Code Block"
  where
    identcode : String -> Parser (Block Star)
    identcode m = do
      ss <- many $ (string m $!> manyTill (anyChar) eol)
      eol
      let src = concatMap (\x => pack (x ++ ['\n'])) ss
      pure $ VerbBlock LiteralTy Nothing Nothing Nothing src
     <?> "Indented Code"

fencedcode : Parser (Block Star)
fencedcode = fencedcode' "```" <|> fencedcode' "~~~" <?> "Fenced Code Block"
  where
    fencedcode' : String -> Parser (Block Star)
    fencedcode' m = do
        string m
        srcopts <- opt $ char ' ' $> manyTill (anyChar) eol
        let as = dealWithSrcAttrs (convertOpts srcopts) Nothing
        src <- manyTill anyChar (string m)
        eol
        pure $ VerbBlock ListingTy Nothing Nothing as (pack src)
      <?> "Fenced Code Block: " ++ m

blockquote : Parser (Block Star)
blockquote = do
    txt <- some $ (token ">" $!> manyTill inline eol)
    eol
    let p = concat txt
    pure $ TextBlock QuotationTy Nothing Nothing Nothing p

-- ------------------------------------------------------------------- [ Paras ]
para : Parser (Block Star)
para = do
    txt <- manyTill (inline) (eol $> eol)
    pure $ TextBlock ParaTy Nothing Nothing Nothing txt
  <?> "Paragraphs"

paraLast : Parser (Block Star)
paraLast = do
    txt <- manyTill inline (eol $> space)
    pure $ TextBlock ParaTy Nothing Nothing Nothing txt
  <?> "Filthy hack for last para"

hrule : Parser (Block Star)
hrule = hrule' "***" <|> hrule' "---" <|> hrule' "___" <?> "hrules"
  where
    hrule' m = do
      string m
      eol
      pure $ HRule Star

header : Parser (Block Star)
header = char '#' >! do
    depth <- opt (many $ char '#')
    space
    title <- manyTill (inline) (eol)
    eol
    let d = length (fromMaybe [] depth) + 1
    pure (Header Star d Nothing title)
  <?> "Header"

block : Parser (Block Star)
block = header
    <|> blockquote <|> indentedcode <|> fencedcode
    <|> list <|> figure <|> hrule <|> para
    <?> "Block"

parseCommonMark : Parser (Edda Star)
parseCommonMark = do
    txt <- some (block)
    lpara <- many paraLast
    let txt' = intersperse (Empty Star) txt
    pure $ MkEddaRaw Nothing (txt' ++ [Empty Star] ++ lpara)
  <?> "Raw Common Mark"

-- -------------------------------------------------------------------- [ Read ]
public
readCommonMark : String -> {[FILE_IO ()]} Eff (Either String EddaDoc)
readCommonMark = readEddaFile parseCommonMark
