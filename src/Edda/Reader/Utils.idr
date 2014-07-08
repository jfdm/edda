module Edda.Reader.Utils

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

%access public

{-
comma : Parser ()
comma = token "," <?> "Comma"

equals : Parser ()
equals = token "=" <?> "equals"

dot : Parser ()
dot = token "." <?> "dot"

colon : Parser ()
colon = token ":" <?> "colon"

semi : Parser ()
semi = token ";" <?> "semi colon"

-}
hash : Parser ()
hash = token "#"

plus : Parser ()
plus = token "+"
{-
simpleLex : Monad m => ParserT m String a -> ParserT m String a
simpleLex p = do
    x <- p
    space
    return x

between : Monad m =>
           (open : ParserT m str a)
        -> (close : ParserT m str a)
        -> (p : ParserT m str b)
        -> ParserT m str b
between open close p = open $!> p <$ close

brackets : Monad m => ParserT m String a -> ParserT m String a
brackets p = between (token "[") (token "]") (lexme p)

braces : Monad m => ParserT m String a -> ParserT m String a
braces p = between (token "{") (token "}") (lexme p)

angles : Monad m => ParserT m String a -> ParserT m String a
angles p = between (token "<") (token ">") (lexme p)

squote : Monad m => ParserT m String a -> ParserT m String a
squote p = between (char '\'') (char '\'') (lexme p)

dquote : Monad m => ParserT m String a -> ParserT m String a
dquote p = between (char '\"') (char '\"') (lexme p)
-}

isPunc : Char -> Bool
isPunc c = List.elem c [',', '!', '?', '<', '>', ':', ';', '.', '-']

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
filepath = lexme $ map pack (some pathChar)
         <?> "filepath"

fileLink : Parser String
fileLink = lexme $ brackets filepath
--

private
asciiChar : Parser Char
asciiChar = satisfy isAlphaNum
        <|> satisfy isDigit
        <|> satisfy isPunc

raw : Parser String
raw = lexme $ map pack (some asciiChar)

identifier : Parser String
identifier = lexme $ map pack $ many (satisfy isAlphaNum)
             <?> "Identifier"

-- The following is 'inspired' from Bibdris

charLetter : Parser Char
charLetter = satisfy (/= '\'')

charLiteral : Parser Char
charLiteral = lexme $ squote charLetter

stringLetter : Parser Char
stringLetter = satisfy (/= '"')

stringLiteral : Parser String
stringLiteral = lexme $ map pack $ dquote (many stringLetter)
