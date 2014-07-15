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
    '|'  => map (RawPunc Pipe) (pure '|')
    x    => map (RawPunc Other) (pure x)
 <?> "Raw Punctuation"

rawOrg : Parser (Inline Raw)
rawOrg =  rawWord  <|> rawPuncSpecial <|> rawPunc <?> "Raw Inline"

eddaOrgRawReader : Parser EddaRaw
eddaOrgRawReader = do
  txt <- some rawOrg
  pure $ MkEddaRaw Nothing txt
 <?> "Raw Org Mode"
