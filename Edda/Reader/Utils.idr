-- --------------------------------------------------------------- [ Utils.idr ]
-- Module    : Utils.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Edda.Reader.Utils

import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Edda.Model
import Edda.Utils

%access export

-- ------------------------------------------------------------- [ Combinators ]

lexemeL : Monad m => ParserT m String a -> ParserT m String a
lexemeL p = space *> p

lexL : Monad m => ParserT m String a -> ParserT m String a
lexL p = lexemeL p

lex : Monad m => ParserT m String a -> ParserT m String a
lex p = lexeme p

-- ---------------------------------------------------------- [ String Parsers ]

char' : Char -> Parser ()
char' c = char c *> return ()

private
pathChar : Parser Char
pathChar = urlChar <|> satisfy (isAlphaNum) <?> "Path Char"
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
word = map pack (some alphaNum) <?> "Word"

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

-- ----------------------------------------------------------------- [ Parsers ]

punc : Parser (Edda STAR INLINE)
punc = map Punc punctuation <?> "Raw Punctuation"

text : Parser (Edda STAR INLINE)
text = map (Font SerifTy) word <?> "Raw Word"

rsvp : List Char
rsvp = ['+', '=', '*', '/', '~', '_']

borderPunc : Parser (Char)
borderPunc = do
    c <- punctuation
    case c of
      ','  => satisfy (const False)
      '\'' => satisfy (const False)
      '\"' => satisfy (const False)
      x    => if x `elem` rsvp
                then satisfy (const False)
                else pure x

mText : Parser (Edda STAR INLINE)
mText = text <|> map Punc borderPunc <?> "Texted used in markup"

-- --------------------------------------------------------------------- [ EOF ]
