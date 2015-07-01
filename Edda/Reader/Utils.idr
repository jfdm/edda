-- --------------------------------------------------------------- [ Utils.idr ]
-- Module    : Utils.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Edda.Reader.Utils

import public Lightyear
import public Lightyear.Strings

import Edda.Model
import Edda.Utils

%access public

-- ------------------------------------------------------------- [ Combinators ]

manyTill : Monad m => ParserT m String a -> ParserT m String b -> ParserT m String (List a)
manyTill p end = scan
  where
    scan : Monad m => ParserT m String (List a)
    scan = do { end; return List.Nil } <|>
           do { x <- p; xs <- scan; return (x::xs)}

lexemeL : Monad m => ParserT m String a -> ParserT m String a
lexemeL p = space *> p

lexL : Monad m => ParserT m String a -> ParserT m String a
lexL p = lexemeL p

lex : Monad m => ParserT m String a -> ParserT m String a
lex p = lexeme p

-- ---------------------------------------------------------- [ String Parsers ]

literallyBetween : Char -> Parser String
literallyBetween c = map pack $ between (char c) (char c) (some (satisfy (/= c)))

eol : Parser ()
eol = char '\n' *> return ()

char' : Char -> Parser ()
char' c = char c *> return ()

anyChar : Parser Char
anyChar = satisfy (const True)

private
pathChar : Parser Char
pathChar = urlChar <|> satisfy isAlphaNum <?> "Path Char"
  where
    urlChar : Parser Char
    urlChar = do
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

url : Parser String
url = map pack (some pathChar) <?> "URL"

word : Parser String
word = map pack (some $ satisfy isAlphaNum) <?> "Word"

punctuation : Parser Char
punctuation = satisfy (\x => not $ isAlphaNum x) <?> "Punctuation"

-- -------------------------------------------------------------- [ Misc Stuff ]

dealWithSrcAttrs : Maybe String
              -> (List (String, String))
              -> (List (String, String))
dealWithSrcAttrs Nothing         as = as
dealWithSrcAttrs (Just srcattrs) as = srcLang ++ srcOpts ++ as
  where
    foo : (String, String)
    foo = break (== ' ') srcattrs

    srcLang : (List (String, String))
    srcLang = [("src_lang", fst foo)]
    srcOpts : (List (String, String))
    srcOpts = [("src_opts", trim $ snd foo)]

convertOpts : Maybe (List Char) -> Maybe String
convertOpts b = case b of
                  Just x => Just (pack x)
                  Nothing => Nothing

convertAttrs : Maybe (String, String) -> List (String, String)
convertAttrs Nothing  = Nil
convertAttrs (Just x) = [x]

-- --------------------------------------------------------------------- [ EOF ]
