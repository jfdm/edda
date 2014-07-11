module Edda.Reader.Org

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Edda.Model
import Edda.Utils

import Edda.Reader.Utils
import Edda.Reader.OrgUtils -- Nasty Quote parsing lives there

-- ------------------------------------------------------------------ [ Inline ]
literallyBetween : Char -> Parser String
literallyBetween c = map pack $ between (char c) (char c) (some (satisfy (/= c)))

markup : Char -> Parser Inline
markup c = do
    txt <- literallyBetween c
    pure $ Serif txt

inlineLink : Parser Inline
inlineLink = do
    txt <- brackets fileLink
    pure $ treatLink txt txt

link : Parser Inline
link = do
    token "["
    url <- fileLink
    txt <- brackets (many word)
    token "]"
    space
    pure $ treatLink url (unwords txt)

text : Parser Inline
text = do
    txt <- some raw
    pure $ Serif $ unwords txt

inline : Parser Inline
inline = inline'
  where
    inline' : Parser Inline
    inline' = (map Emph (markup '/'))
                   <|> (map Strong (markup '*'))
                   <|> (map Verb (markup '~'))
                   <|> (map Strike (markup '+'))
                   <|> (map CodeSnippet (literallyBetween '='))
                   <|> (map MathSnippet (literallyBetween '$'))
                   <|> inlineLink
                   <|> link
                   <|> map (\x => Quote SQuote (Serif x)) (map pack $ squote (many (satisfy (/= '\''))))
                   <|> map (\x => Quote DQuote (Serif x)) stringLiteral
                   <|> text
                   <?> "Markup"

-- ---------------------------------------------------------------- [ Property ]

optionLine : String -> Parser Property
optionLine key = do
    string "#+" $> string key
    colon
    ps <- (manyTill anyChar eol)
    space
    pure (key, pack ps)
  <?> "Options Property"


property : String -> Parser Property
property key = do
    string "#+" $> string key
    colon
    ps <- (some word)
    pure (key, unwords ps)
  <?> "Formatted Property"

-- ------------------------------------------------------------------ [ Blocks ]

caption : Parser (List Inline)
caption = do
    token "#+CAPTION:"
    ps <- some $ lexeme inline
    pure ps
  <?> "Caption"

label : Parser String
label = do
    (k,v) <- optionLine "NAME"
    pure v
  <?> "Label"

figure : Parser Block
figure = do
    cap <- caption
    lab <- label
    as  <- opt $ some (property "ATTR")
    img <- inlineLink
    let i = MkImage as "" img
    pure (Figure lab cap i)
  <?> "Figure"

listing : Parser Block
listing = do
    cap <- opt $ caption
    lab <- opt $ label
    token "#+BEGIN_SRC" >! do
      t <- word
      txt <- manyTill anyChar (token "#+END_SRC")
      pure $ Listing lab cap (Just [("lang", t)]) (pack txt)
  <?> "Listing"

quotation : Parser Block
quotation = do
    txt <- between (token "#+BEGIN_QUOTE") (token "#+END_QUOTE") (some $ lexeme inline)
    pure $ Quotation txt
  <?> "Quotation"

verse : Parser Block
verse = do
    txt <- between (token "#+BEGIN_VERSE") (token "#+END_VERSE") (some $ lexeme inline)
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
    ty <- raw
    space
--    let ats = Just [("type", word)]
    case readTheorem ty of
        Just thm => do
          txt <- manyTill (lexeme inline) (string "#+END_" $> token ty)
          pure $ Theorem lab cap thm txt
        Nothing => do
          txt <- manyTill anyChar (string "#+END_" $> token ty)
          pure $ Generic lab cap as (pack txt)


header : Parser Block
header = char '*' >! do
    depth <- opt (some astrix)
    space
    ws <- many word
    let d = length (fromMaybe [] depth) + 1
    let title = unwords ws
    pure $ Heading d "" [Serif title]
  <?> "Heading"

para : Parser Block
para = do
    t <- inline
    xt <- manyTill (space $> inline) (string "\n\n")
    pure $ Para (t :: xt)
  <?> "Paragraphs"

-- Table
-- Lists OL UL DT
block : Parser Block
block =  block'
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
  title <- property "TITLE"
  author <- property "AUTHOR"
  date <- property "DATE"
  opts <- opt $ many $ optionLine "OPTS"
  body <- some block
  let ps = [title, author, date] ++ fromMaybe [] opts
  pure $ MkEdda (Just ps) body
