module Edda.Reader.Utils

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

%access public

manyTill : Monad m => ParserT m str a -> ParserT m str b -> ParserT m str (List a)
manyTill p end = scan
  where
    scan = do { end; return List.Nil } <|>
           do { x <- p; xs <- scan; return (x::xs)}

lexemeL : Monad m => ParserT m String a -> ParserT m String a
lexemeL p = space $> p

lexL : Monad m => ParserT m String a -> ParserT m String a
lexL p = lexemeL p

lex : Monad m => ParserT m String a -> ParserT m String a
lex p = lexeme p

hash : Parser ()
hash = char '#'

astrix : Parser ()
astrix = char '*'

plus : Parser ()
plus = char '+'

eol : Parser ()
eol = char '\n'

anyChar : Parser Char
anyChar = satisfy (const True)

isPunc : Char -> Bool
isPunc c = List.elem c [',', '!', '?', '<', '>', ':', ';', '.', '-', '\'']

-- Inspired by Json.idr in Lightyear examples

private
specialChar : Parser Char
specialChar = do
  c <- satisfy (const True)
  case c of
    '\\' => pure '\\'
    '/'  => pure '/'
    '.'  => pure '.'
    ':'  => pure ':'
    '#'  => pure '#'
    '='  => pure '='
    '?'  => pure '?'
    '-'  => pure '-'
    _    => satisfy (const False)

private
pathChar : Parser Char
pathChar = specialChar
       <|> satisfy isAlphaNum
       <|> satisfy isDigit

filepath : Parser String
filepath = map pack (some pathChar)
         <?> "filepath"

fileLink : Parser String
fileLink = brackets filepath
--

private
asciiChar : Parser Char
asciiChar = satisfy isAlphaNum
        <|> satisfy isDigit
        <|> satisfy isPunc

word : Parser String
word = map pack (some $ satisfy isAlphaNum)

punc : Parser Char
punc = satisfy (\x => not $isAlphaNum x)

--word : Parser String
--word = map pack (some asciiChar)

--word : Parser String
--word = lexeme raw

identifier : Parser String
identifier = lexeme $ map pack $ many (satisfy isAlphaNum)
             <?> "Identifier"

-- The following is 'inspired' from Bibdris

charLetter : Parser Char
charLetter = satisfy (/= '\'')

charLiteral : Parser Char
charLiteral = squote charLetter

stringLetter : Parser Char
stringLetter = satisfy (/= '"')

stringLiteral : Parser String
stringLiteral = map pack $ dquote (many stringLetter)
