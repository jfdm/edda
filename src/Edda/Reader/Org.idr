module Edda.Reader.Org

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Edda.Model
import Edda.Utils

import Edda.Reader.Utils
import Edda.Reader.OrgUtils -- Nasty Quote parsing lives there

-- ------------------------------------------------------------------ [ Inline ]
betweenLit : Char -> Parser String
betweenLit c = lexme $ map pack $ between (char c) (char c) (many (satisfy (/= c)))

parseMarkup : Char -> Parser Inline
parseMarkup c = do
    txt <- betweenLit c
    pure $ Serif txt

parseLinkInline : Parser Inline
parseLinkInline = do
    txt <- brackets fileLink
    pure $ treatLink txt txt

parseLink : Parser Inline
parseLink = do
    token "["
    url <- filepath
    txt <- brackets (many raw)
    token "]"
    space
    pure $ treatLink url (unwords txt)

parseText : Parser Inline
parseText = do
    txt <- some raw
    pure $ Serif $ unwords txt

parseFormatting' : Parser Inline
parseFormatting' = (map Emph (parseMarkup '/'))
               <|> (map Strong (parseMarkup '*'))
               <|> (map Verb (parseMarkup '~'))
               <|> (map Strike (parseMarkup '+'))
               <|> (map CodeSnippet (betweenLit '='))
               <|> (map MathSnippet (betweenLit '$'))
               <|> map (\x => Quote SQuote (Serif x)) (map pack $ squote (many (satisfy (/= '\''))))
               <|> map (\x => Quote DQuote (Serif x)) stringLiteral
               <|> parseLinkInline
               <|> parseLink
               <|> parseText
               <?> "Markup"

parseInline : Parser Inline
parseInline = lexme $ parseFormatting'

-- ------------------------------------------------------------------ [ Blocks ]

parsePlain : Parser Block
parsePlain = do
    txt <- many parseInline <$ space
    pure $ Plain txt

parseHeader : Parser Block
parseHeader = char '*' >! do
    d <- many $ char '*' <$ space
    title <- many parseInline <$ space
    let depth = length d
    pure $ Heading (depth + 1) "a" title

parseBlock : Parser Block
parseBlock = space $> parseBlock' <$ space
  where
    parseBlock' : Parser Block
    parseBlock' = parseHeader
              <|> parsePlain
              <?> "Blocks"

-- ---------------------------------------------------------------- [ Property ]

parseProperty : Parser Property
parseProperty = char '#' $> char '+' >! do
    key <- map pack $ many (satisfy isAlpha)
    char ':'
    space
    value <- many identifier <$ space
    pure (key, unwords value)
  <?> "Property"

-- --------------------------------------------------------------------- [ Org ]

eddaOrgReader : Parser Edda
eddaOrgReader = do
  ps <- many parseProperty
  body <- many parseBlock
  pure $ MkEdda (Just ps) body
