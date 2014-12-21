module Edda.Reader.Utils

import public Control.Monad.Identity
import public Lightyear.Core
import public Lightyear.Combinators
import public Lightyear.Strings

import Edda.Model
import Edda.Utils

%access public

-- ------------------------------------------------------------- [ Combinators ]

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

brackets' : Monad m => ParserT m String a -> ParserT m String a
brackets' p = between (char '[') (char ']') p

literallyBetween : Char -> Parser String
literallyBetween c = map pack $ between (char c) (char c) (some (satisfy (/= c)))

-- ---------------------------------------------------------- [ String Parsers ]

eol : Parser ()
eol = char '\n'

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
              -> Maybe Attributes
              -> Maybe Attributes
dealWithSrcAttrs Nothing         Nothing   = Nothing
dealWithSrcAttrs Nothing         (Just as) = Just as
dealWithSrcAttrs (Just srcattrs) as        = Just $ srcLang ++ srcOpts ++ fromMaybe [] as
  where
    foo : (String, String)
    foo = break (== ' ') srcattrs

    srcLang : Attributes
    srcLang = [("src_lang", fst foo)]
    srcOpts : Attributes
    srcOpts = [("src_opts", trim $ snd foo)]

convertOpts : Maybe (List Char) -> Maybe String
convertOpts b = case b of
                  Just x => Just (pack x)
                  Nothing => Nothing

convertAttrs : Maybe Attribute -> Maybe Attributes
convertAttrs Nothing  = Nothing
convertAttrs (Just x) = Just [x]

-- --------------------------------------------------------------------- [ EOF ]
