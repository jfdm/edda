module Edda.Reader.Utils

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Edda.Model

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

literallyBetween : Char -> Parser String
literallyBetween c = map pack $ between (char c) (char c) (some (satisfy (/= c)))

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

isVerbBlock : String -> Bool
isVerbBlock bTy = List.elem bTy ["COMMENT", "SRC", "EXAMPLE"]

convertOpts : Maybe (List Char) -> Maybe String
convertOpts b = case b of
                  Just x => Just (pack x)
                  Nothing => Nothing

dealWithAttrs :  String
              -> Maybe String
              -> Maybe Attributes
              -> Attributes
dealWithAttrs ty srcOpts as = [("type", ty)]
                            ++ fooOpts "src_opts" srcOpts
                            ++ fromMaybe [] as
  where
    fooOpts tag (Just opts) = [(tag, opts)]
    fooOpts tag Nothing     = []
