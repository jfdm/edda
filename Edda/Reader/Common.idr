module Edda.Reader.Common

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Effects
import Effect.File
import Effect.StdIO

import Edda.Model
import Edda.Refine

import Edda.Reader.Utils

%access public

-- ----------------------------------------------------------------- [ Parsers ]

punc : Parser (Inline Star)
punc = map Punc punctuation <?> "Raw Punctuation"

text : Parser (Inline Star)
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

mText : Parser (Inline Star)
mText = text <|> map Punc borderPunc <?> "Texted used in markup"

-- ------------------------------------------------------------------ [ Reader ]

readFile : { [FILE_IO (OpenFile Read)] } Eff String
readFile = readAcc ""
  where
    readAcc : String -> { [FILE_IO (OpenFile Read)] } Eff String
    readAcc acc = if (not !eof)
                     then readAcc (acc ++ !readLine)
                     else pure acc

public
readEddaFile : Parser EddaRaw -> String -> { [FILE_IO ()] } Eff (Either String EddaDoc)
readEddaFile p f = do
    case !(open f Read) of
      True => do
        src <- readFile
        close
        case parse p src of
          Left err  => pure $ Left err
          Right res => pure $ Right (refineEdda res)
      False => pure $ Left "Error"
-- --------------------------------------------------------------------- [ EOF ]
