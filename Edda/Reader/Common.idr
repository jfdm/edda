-- -------------------------------------------------------------- [ Common.idr ]
-- Module    : Common.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Edda.Reader.Common

import Lightyear
import Lightyear.Strings

import Effects
import Effect.File
import Effect.StdIO

import Edda.Model
import Edda.Refine

import Edda.Reader.Utils

%access public

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

-- ------------------------------------------------------------------ [ Reader ]

readFile : { [FILE_IO (OpenFile Read)] } Eff String
readFile = readAcc ""
  where
    readAcc : String -> { [FILE_IO (OpenFile Read)] } Eff String
    readAcc acc = if (not !eof)
                     then readAcc (acc ++ !readLine)
                     else pure acc

public
readEddaFile : Parser (Edda STAR MODEL) -> String -> { [FILE_IO ()] } Eff (Either String (Edda PRIME MODEL))
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
