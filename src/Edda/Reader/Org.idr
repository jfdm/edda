module Edda.Reader.Org

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Edda.Model
import Edda.Utils

import Edda.Reader.Utils
import Edda.Reader.OrgUtils -- Nasty Quote parsing lives there

%access public

-- ------------------------------------------------------------------ [ Inline ]
private
literallyBetween : Char -> Parser String
literallyBetween c = map pack $ between (char c) (char c) (some (satisfy (/= c)))

code : Parser Inline
code = map CodeSnippet (literallyBetween '~') <?> "Code"

verb : Parser Inline
verb = map Verbatim (literallyBetween '=') <?> "Verb"

math : Parser Inline
math = map MathSnippet (literallyBetween '$') <?> "Math"

text : Parser Inline
text = map Serif word <?> "Text"

mutual
  marker : Char -> Parser (List Inline)
  marker c = between (char c)
                     (char c)
                     (do {x <- text; pure (the (List Inline) [x])})

  bold : Parser Inline
  bold = map Strong $ marker '*' <?> "Bold"

  italic : Parser Inline
  italic = map Emph $ marker '/' <?> "Italic"

  strike : Parser Inline
  strike = map Strike $ marker '+' <?> "Strike"

  uline : Parser Inline
  uline = map Uline $ marker '_' <?> "Underline"

  markup : Parser Inline
  markup = verb <|> code   <|> math
       <|> bold <|> italic <|> strike <|> uline
       <?> "Markup"

  inlineLink : Parser Inline
  inlineLink = do
    txt <- brackets fileLink
    pure $ treatLink txt [Serif txt]
   <?> "Inline Link"

  hyperLink : Parser Inline
  hyperLink = do
    token "["
    url <- fileLink
    txt <- brackets (some $ lexL text)
    token "]"
    pure $ treatLink url txt
   <?> "Hyperlink"

  fnote : Parser Inline
  fnote = do
    string "[fn"
    label <- literallyBetween ':'
    desc <- some $ lexL text
    string "]"
    pure $ FNote label desc
   <?> "Footnote"

  link : Parser Inline
  link = inlineLink <|> hyperLink <|> fnote <?> "Link"

  misc : Parser Inline
  misc = map (\x => Quote DQuote x)    (marker '\"')
--     <|> map (\x => Quote SQuote x)    (marker '\'')
     <|> map (\x => Parens Parents x)  (parens   $ some $ lex markup)
     <|> map (\x => Parens Brackets x) (brackets $ some $ lex markup)
     <|> map (\x => Parens Braces x)   (braces   $ some $ lex markup)
     <?> "Misc formatting"

  inline : Parser Inline
  inline = link <|> markup <|> misc <|> text <?> "Inline"

-- -------------------------------------------------------------- [ Properties ]

optionLine : String -> Parser Property
optionLine key = do
    string "#+" $> string key
    colon
    ps <- (manyTill anyChar eol)
    pure (key, pack ps)
  <?> "Options Property"

property : String -> Parser Property
property key = do
    string "#+" $> string key
    colon
    space
    ps <- manyTill (lexL word) eol
    pure (key, unwords ps)
  <?> "Formatted Property"

caption : Parser (List Inline)
caption = do
    token "#+CAPTION:"
    ps <- some $ lex inline
    pure ps
  <?> "Caption"

label : Parser String
label = do
    (k,v) <- optionLine "NAME"
    pure v
  <?> "Label"

-- ------------------------------------------------------------------ [ Blocks ]

figure : Parser Block
figure = do
    cap <- caption
    lab <- label
    as  <- opt $ some (property "ATTR")
    img <- lex inlineLink
    let i = MkImage as "" img
    pure (Figure lab cap i)
  <?> "Figure"

listing : Parser Block
listing = do
    cap <- opt $ caption
    lab <- opt $ label
    string "#+BEGIN_SRC" >! do
      space
      t <- word
      space
      txt <- manyTill anyChar (eol $> token "#+END_SRC")
      pure $ Listing lab cap (Just [("lang", t)]) (pack txt)
  <?> "Listing"

quotation : Parser Block
quotation = do
    txt <- between (token "#+BEGIN_QUOTE")
                   (token "#+END_QUOTE")
                   (some $ lex inline)
    pure $ Quotation txt
  <?> "Quotation"

verse : Parser Block
verse = do
    txt <- between (token "#+BEGIN_VERSE")
                   (token "#+END_VERSE")
                   (some $ lex inline)
    pure $ Quotation txt
  <?> "Verse"

equation : Parser Block
equation = do
    lab <- opt (label)
    token "#+BEGIN_EQUATION" >! do
      eq <- manyTill anyChar (token "#+END_EQUATION")
      pure $ Equation lab (pack eq)
  <?> "equation"

generic : Parser Block
generic = do
    cap <- opt caption
    lab <- opt label
    as  <- opt $ some (property "ATTR")
    string "#+BEGIN_"
    ty <- word
    space
--    let ats = Just [("type", word)]
    case readTheorem ty of
        Just thm => do
          txt <- manyTill (lex inline) (string "#+END_" $> token ty)
          pure $ Theorem lab cap thm txt
        Nothing => do
          txt <- manyTill anyChar (string "#+END_" $> token ty)
          pure $ Generic lab cap as (pack txt)
   <?> "Blocks"


header : Parser Block
header = astrix >! do
    depth <- opt (many astrix)
    title <- manyTill (lexL inline) (eol $> space)
    let d = length (fromMaybe [] depth) + 1
    --let t = map Serif title
    pure $ Heading d "" title
  <?> "Heading"


para : Parser Block
para = do
    xt <- manyTill (lexL inline) (eol $> space)
    pure $ Para (xt)
  <?> "Paragraphs"


-- Table
-- Lists OL UL DT
block : Parser Block
block =  block' <$ space
  where
    block' : Parser Block
    block' = header
         <|> quotation
         <|> verse
         <|> equation
         <|> figure
         <|> listing
         <|> generic
         <|> para
         <?> "Blocks"

-- --------------------------------------------------------------------- [ Org ]

eddaOrgReader : Parser Edda
eddaOrgReader = do
  title  <- property "TITLE"
  author <- property "AUTHOR"
  date   <- property "DATE"
  body   <- space $> many block
  let ps = the (List Property) [title, author, date]
  pure $ MkEdda (Just ps) body
