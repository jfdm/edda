-- -------------------------------------------------------------- [ Common.idr ]
-- Module    : Common.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Edda.Reader.Common

import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Lightyear.StringFile

import Effects
import Effect.File
import Effect.StdIO

import Edda.Model
import public Edda.Refine

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


public
readEddaFile : Parser (Edda STAR MODEL)
            -> String
            -> Eff (Either String (Edda PRIME MODEL)) [FILE_IO ()]
readEddaFile p f = do
    c <- parseFile (\x => x) (\x,y => unwords [x,y]) p f
    case  c of
      Left err  => pure $ Left err
      Right res => pure $ Right (refineEdda res)

readEddaSentance : Parser (Edda STAR INLINE)
                -> String
                -> Either String EddaString
readEddaSentance p s =
  case parse (some p) s of
    Left err  => Left err
    Right res => Right $ refineInlines res

readEddaBody : Parser (Edda STAR BLOCK)
        -> String
        -> Either String (List EddaBlock)
readEddaBody p ps =
  case parse (some p) ps of
    Left err  => Left err
    Right res => Right $ refineBlocks res

-- --------------------------------------------------------------------- [ EOF ]
