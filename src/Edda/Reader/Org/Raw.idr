module Edda.Reader.Org.Raw

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Edda.Model
import Edda.Utils

import Edda.Reader.Utils

%access public
-- --------------------------------------------------------------------- [ Org ]

rawWord : Parser (Inline Raw)
rawWord = map RawText word <?> "Raw Word"

rawPuncSpecial : Parser (Inline Raw)
rawPuncSpecial = rawEmDash <|> rawEnDash <|> rawEllipsis <?> "Raw Special Punc"
  where
    rawEnDash = do
      ntimes 2 (char '-')
      pure $ RawPunc EnDash '-'
    rawEmDash = do
      ntimes 3 (char '-')
      pure $ RawPunc EmDash '-'
    rawEllipsis = do
      ntimes 3 (char '.')
      pure $ RawPunc Ellipsis '.'

rawPunc : Parser (Inline Raw)
rawPunc = do
  c <- punc
  case c of
    ' '  => map (RawPunc Space) (pure ' ')
    '\n' => map (RawPunc Newline) (pure '\n')
    '\t' => map (RawPunc Tab) (pure '\t')
    '{'  => map (RawPunc LBrace) (pure '{')
    '}'  => map (RawPunc RBrace) (pure '}')
    '('  => map (RawPunc LParen) (pure '(')
    ')'  => map (RawPunc RParen) (pure ')')
    '['  => map (RawPunc LBrack) (pure '[')
    ']'  => map (RawPunc RBrack) (pure ']')
    '<'  => map (RawPunc LAngle) (pure '<')
    '>'  => map (RawPunc RAngle) (pure '>')
    ':'  => map (RawPunc Colon) (pure ':')
    ';'  => map (RawPunc Semi) (pure ';')
    '/'  => map (RawPunc FSlash) (pure '/')
    '\\' => map (RawPunc BSlash)(pure '\\')
    '\'' => map (RawPunc Apostrophe) (pure '\'')
    '\"' => map (RawPunc SMark) (pure '\"')
    '-'  => map (RawPunc Hyphen) (pure '-')
    ','  => map (RawPunc Comma) (pure ',')
    '+'  => map (RawPunc Plus) (pure '+')
    '!'  => map (RawPunc Bang) (pure '!')
    '.'  => map (RawPunc Period) (pure '.')
    '?'  => map (RawPunc QMark) (pure '?')
    '#'  => map (RawPunc Hash) (pure '#')
    '='  => map (RawPunc Equals) (pure '=')
    '$'  => map (RawPunc Dollar) (pure '$')
    '|'  => map (RawPunc Pipe) (pure '|')
    x    => map (RawPunc Other) (pure x)
 <?> "Raw Punctuation"

rawInline : Parser (Inline Raw)
rawInline =  rawWord  <|> rawPuncSpecial <|> rawPunc <?> "Raw Inline"

-- -------------------------------------------------------------- [ Properties ]
-- @TODO Factorise this.

rawAttribute : String -> Parser (String, String)
rawAttribute key = do
    string "#+" $> string key
    colon
    ps <- manyTill (anyChar) eol
    pure (key, pack ps)
  <?> "Raw Attribute"

rawInlineKeyWord : String -> Parser (String, RawListInline)
rawInlineKeyWord key = do
    string "#+" $> string key
    colon
    space
    ps <- manyTill (rawInline) eol
    pure (key, ps)
  <?> "Attribute."

caption : Parser RawListInline
caption = do
    (k,v) <- rawInlineKeyWord "CAPTION"
    pure v
  <?> "Caption"

label : Parser String
label = do
    (k,v) <- rawAttribute "NAME"
    pure v
  <?> "Label"

-- ------------------------------------------------------------------ [ Blocks ]

foobar : String -> Maybe String -> Maybe Attributes
       -> Attributes
foobar ty srcOpts as = [("type", ty)]
                            ++ fooOpts "src_opts" srcOpts
                            ++ fromMaybe [] as
  where
    fooOpts tag (Just opts) = [(tag, opts)]
    fooOpts tag Nothing     = []

bar : Maybe (List Char) -> Maybe String
bar b = case b of
          Just x => Just (pack x)
          Nothing => Nothing

rawBlock : Parser BlockRaw
rawBlock = do
    cap <- opt caption
    lab <- opt label
    rawAs  <- opt $ some (rawAttribute "ATTR")
    string "#+BEGIN_"
    ty <- word
    bopts <- opt $ char ' ' $> manyTill (anyChar) eol
    let as = foobar ty (bar bopts) rawAs
    case readTheorem ty of
      Just thm => do
        txt <- manyTill rawInline (string "#+END_" $> token ty)
        pure $ RawBlock lab cap (Just as) (Right txt)
      Nothing => do
        txt <- manyTill anyChar (string "#+END_" $> token ty)
        pure $ RawBlock lab cap (Just as) (Left $ pack txt)
   <?> "Raw Blocks"

rawFigure : Parser BlockRaw
rawFigure = do
    cap <- caption
    lab <- label
    as  <- opt $ some (rawAttribute "ATTR")
    img <- manyTill (rawInline) eol
    space
    pure (RawFigure lab cap as img)
  <?> "Raw Figure"

rawPara : Parser BlockRaw
rawPara = do
    xt <- manyTill rawInline (eol $> eol)
    space
    pure $ RawPara xt
  <?> "Raw Paragraphs"

rawParaLast : Parser BlockRaw
rawParaLast = do
    xt <- manyTill rawInline (eol $> space)
    pure $ RawPara xt

rawHeader : Parser BlockRaw
rawHeader = astrix >! do
    depth <- opt (many astrix)
    space
    title <- manyTill (rawInline) (eol $> space)
    let d = length (fromMaybe [] depth) + 1
    pure (RawHeader d "" title)

rawOrg : Parser BlockRaw
rawOrg = rawHeader <|> rawBlock <|> rawFigure <|> rawPara

eddaOrgRawReader : Parser EddaRaw
eddaOrgRawReader = do
  title  <- rawAttribute "TITLE"
  author <- rawAttribute "AUTHOR"
  date   <- rawAttribute "DATE"
  txt    <- many rawOrg
  lpara  <- many rawParaLast -- Dirty Hack
  let ps = the Attributes [title, author, date]
  pure $ MkEddaRaw (Just ps) (txt ++ lpara)
 <?> "Raw Org Mode"
