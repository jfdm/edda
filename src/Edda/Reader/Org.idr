module Edda.Reader.Org

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Edda.Model
import Edda.Utils
import Edda.Reduce

import Edda.Reader.Utils

%access public
-- --------------------------------------------------------------------- [ Org ]

rawWord : Parser (Inline Raw)
rawWord = map (Text Raw) word <?> "Raw Word"

rawPuncSpecial : Parser (Inline Raw)
rawPuncSpecial = rawEmDash <|> rawEnDash <|> rawEllipsis <?> "Raw Special Punc"
  where
    rawEnDash = do
      ntimes 2 (char '-')
      pure $ Punc Raw EnDash '-'
    rawEmDash = do
      ntimes 3 (char '-')
      pure $ Punc Raw EmDash '-'
    rawEllipsis = do
      ntimes 3 (char '.')
      pure $ Punc Raw Ellipsis '.'

rawPunc : Parser (Inline Raw)
rawPunc = do
  c <- punc
  case c of
    ' '  => map (Punc Raw Space)      (pure ' ')
    '\n' => map (Punc Raw Newline)    (pure '\n')
    '\t' => map (Punc Raw Tab)        (pure '\t')
    '{'  => map (Punc Raw LBrace)     (pure '{')
    '}'  => map (Punc Raw RBrace)     (pure '}')
    '('  => map (Punc Raw LParen)     (pure '(')
    ')'  => map (Punc Raw RParen)     (pure ')')
    '['  => map (Punc Raw LBrack)     (pure '[')
    ']'  => map (Punc Raw RBrack)     (pure ']')
    '<'  => map (Punc Raw LAngle)     (pure '<')
    '>'  => map (Punc Raw RAngle)     (pure '>')
    ':'  => map (Punc Raw Colon)      (pure ':')
    ';'  => map (Punc Raw Semi)       (pure ';')
    '/'  => map (Punc Raw FSlash)     (pure '/')
    '\\' => map (Punc Raw BSlash)     (pure '\\')
    '\'' => map (Punc Raw Apostrophe) (pure '\'')
    '\"' => map (Punc Raw SMark)      (pure '\"')
    '-'  => map (Punc Raw Hyphen)     (pure '-')
    ','  => map (Punc Raw Comma)      (pure ',')
    '+'  => map (Punc Raw Plus)       (pure '+')
    '!'  => map (Punc Raw Bang)       (pure '!')
    '.'  => map (Punc Raw Period)     (pure '.')
    '?'  => map (Punc Raw QMark)      (pure '?')
    '#'  => map (Punc Raw Hash)       (pure '#')
    '='  => map (Punc Raw Equals)     (pure '=')
    '$'  => map (Punc Raw Dollar)     (pure '$')
    '|'  => map (Punc Raw Pipe)       (pure '|')
    x    => map (Punc Raw Other)      (pure x)
 <?> "Raw Punctuation"

code : Parser (Inline Raw)
code = map (CodeSnippet Raw) (literallyBetween '~') <?> "Code"

verb : Parser (Inline Raw)
verb = map (Verbatim Raw) (literallyBetween '=') <?> "Verb"

math : Parser (Inline Raw)
math = map (MathSnippet Raw) (literallyBetween '$') <?> "Math"

expLink : Parser (Inline Raw)
expLink = do
    txt <- brackets $ brackets url
    pure $ Link Raw Exposed txt Nothing

hyper : Parser (Inline Raw)
hyper = do
    (uri, desc) <- brackets internal
    pure $ Link Raw Hyperlink uri (Just desc)
  where
    foobar : Parser (Inline Raw)
    foobar = rawWord <|> rawPuncSpecial <|> rawPunc

    internal : Parser (String, RawListInline)
    internal = do
      u <- brackets url
      d <- brackets $ many foobar
      pure (u,d)

link : Parser (Inline Raw)
link = expLink <|> hyper <?> "Link"

rawInline : Parser (Inline Raw)
rawInline =  rawWord
         <|> link <|> code  <|> verb <|> math
         <|> rawPuncSpecial <|> rawPunc
         <?> "Raw Inline"

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

block : Parser (Block Raw)
block = do
    cap <- opt caption
    lab <- opt label
    rawAs  <- opt $ some (rawAttribute "ATTR")
    string "#+BEGIN_"
    ty <- word
    if isVerbBlock ty
      then do
        bopts <- opt $ char ' ' $> manyTill (anyChar) eol
        let as = dealWithAttrs ty (convertOpts bopts) rawAs
        txt <- manyTill anyChar (string "#+END_" $> token ty)
        pure $ VerbBlock Raw lab cap (Just as) (pack txt)
      else do
        txt <- manyTill rawInline (string "#+END_" $> token ty)
        pure $ TextBlock Raw lab cap rawAs txt

   <?> "Raw Blocks"

figure : Parser (Block Raw)
figure = do
    cap <- caption
    lab <- label
    as  <- opt $ some (rawAttribute "ATTR")
    img <- expLink
    space
    pure (Figure Raw lab cap as img)
  <?> "Raw Figure"

para : Parser (Block Raw)
para = do
    xt <- manyTill rawInline (eol $> eol)
    space
    pure $ Para Raw xt
  <?> "Raw Paragraphs"

paraLast : Parser (Block Raw)
paraLast = do
    xt <- manyTill rawInline (eol $> space)
    pure $ Para Raw xt

header : Parser (Block Raw)
header = char '*' >! do
    depth <- opt (many $ char '*')
    space
    title <- manyTill (rawInline) (eol $> space)
    let d = length (fromMaybe [] depth) + 1
    pure (Header Raw d "" title)

rawOrg : Parser (Block Raw)
rawOrg = header <|> block <|> figure <|> para

parseOrg : Parser EddaRaw
parseOrg = do
  title  <- rawAttribute "TITLE"
  author <- rawAttribute "AUTHOR"
  date   <- rawAttribute "DATE"
  txt    <- many rawOrg
  lpara  <- many paraLast -- Dirty Hack
  let ps = the Attributes [title, author, date]
  let txt' = intersperse (Empty Raw) txt
  pure $ MkEddaRaw (Just ps) (txt' ++ [Empty Raw] ++ lpara)
 <?> "Raw Org Mode"
