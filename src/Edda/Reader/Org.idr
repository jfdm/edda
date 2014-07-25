module Edda.Reader.Org

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Edda.Model
import Edda.Utils
import Edda.Reader.Common
import Edda.Reader.Utils

%access public
-- --------------------------------------------------------------------- [ Org ]

rsvp : List Char
rsvp = ['+', '=', '*', '/', '~', '_']

text : Parser (Inline Simple)
text = map (Font SerifTy) word <?> "Raw Word"

punc : Parser (Inline Simple)
punc = map Punc punctuation <?> "Raw Punctuation"

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

mText : Parser (Inline Simple)
mText = text <|> map Punc borderPunc <?> "Texted used in markup"

code : Parser (Inline Simple)
code = map (Raw CodeTy) (literallyBetween '~') <?> "Code"

verb : Parser (Inline Simple)
verb = map (Raw VerbTy) (literallyBetween '=') <?> "Verb"

math : Parser (Inline Simple)
math = map (Raw MathTy) (literallyBetween '$') <?> "Math"

markup : MarkupTy -> Char -> Parser (Inline Simple)
markup mTy c = do
    txt <- between (char c) (char c) (some mText)
    pure $ Mark mTy txt
  <?> "Markup"

bold : Parser (Inline Simple)
bold = markup BoldTy '*' <?> "Bold"

emph : Parser (Inline Simple)
emph = markup EmphTy '/'  <?> "Emph"

strike : Parser (Inline Simple)
strike = markup StrikeTy '+' <?> "Strike"

uline : Parser (Inline Simple)
uline = markup UlineTy '_' <?> "Uline"

expLink : Parser (Inline Simple)
expLink = do
    txt <- brackets $ brackets url
    pure $ Link ExposedTy txt Nothing
  <?> "Exposed Link"

hyper : Parser (Inline Simple)
hyper = do
    (uri, desc) <- brackets internal
    pure $ Link HyperTy uri (Just desc)
  where
    internal : Parser (String, List (Inline Simple))
    internal = do
      u <- brackets url
      d <- brackets $ some text
      pure (u, intersperse (Punc ' ' ) d)

link : Parser (Inline Simple)
link = hyper <|> expLink <?> "Link"

fnote : Parser (Inline Simple)
fnote = do
   (l,d) <- brackets doFnote
   pure $ Link FnoteTy (fromMaybe "" l) d
 where
   doFnote : Parser (Maybe String, Maybe (List (Inline Simple)))
   doFnote = do
     string "fn"
     colon
     lab <- opt word
     colon
     desc <- opt $ some text
     pure (lab, desc)

inline : Parser (Inline Simple)
inline = text
     <|> fnote <|> link
     <|> bold <|> emph <|> strike <|> uline
     <|> code <|> verb <|> math <|> punc
     <?> "Raw Inline"

-- -------------------------------------------------------------- [ Properties ]
attribute : String -> Parser (String, String)
attribute key = do
    string "#+" $> string key
    colon
    ps <- manyTill (anyChar) eol
    pure (key, pack ps)
  <?> "Raw Attribute"

inlineKeyWord : String -> Parser (String, List (Inline Simple))
inlineKeyWord key = do
    string "#+" $> string key
    colon
    space
    ps <- manyTill (inline) eol
    pure (key, ps)
  <?> "Attribute."

caption : Parser (List (Inline Simple))
caption = do
    (k,v) <- inlineKeyWord "CAPTION"
    pure v
  <?> "Caption"

label : Parser String
label = do
    (k,v) <- attribute "NAME"
    pure v
  <?> "Label"

-- ------------------------------------------------------------------ [ Blocks ]

-- @TODO Parse Lists
block : Parser (Block Simple)
block = do
    cap <- opt caption
    lab <- opt label
    rawAs  <- opt $ some (attribute "ATTR")
    string "#+BEGIN_"
    ty <- word
    if isVerbBlock ty
      then do
        bopts <- opt $ char ' ' $> manyTill (anyChar) eol
        let as = dealWithAttrs ty (convertOpts bopts) rawAs
        txt <- manyTill anyChar (string "#+END_" $> token ty)
        pure $ VerbBlock Simple lab cap (Just as) (pack txt)
      else do
        txt <- manyTill inline (string "#+END_" $> token ty)
        pure $ TextBlock Simple lab cap rawAs txt

   <?> "Blocks"

figure : Parser (Block Simple)
figure = do
    cap <- caption
    lab <- label
    as  <- opt $ some (attribute "ATTR")
    img <- expLink
    space
    pure (Figure Simple lab cap as img)
  <?> "Figure"

para : Parser (Block Simple)
para = do
    xt <- manyTill inline (eol $> eol)
    space
    pure $ Para Simple xt
  <?> "Paragraphs"

paraLast : Parser (Block Simple)
paraLast = do
    xt <- manyTill inline (eol $> space)
    pure $ Para Simple xt
  <?> "Filthy hack for last para"

header : Parser (Block Simple)
header = char '*' >! do
    depth <- opt (many $ char '*')
    space
    title <- manyTill (inline) (eol $> space)
    let d = length (fromMaybe [] depth) + 1
    pure (Header Simple d "" title)


ulMarker : Parser ()
ulMarker = char '+' <|> char '-' <?> "UList Marker"

olMarker : Parser ()
olMarker = marker '.' <|> marker ')'
  where
    marker : Char -> Parser ()
    marker c = do
      some $ satisfy (isDigit)
      char c
      (satisfy isSpace)
      pure ()

-- @TODO Add coninuations
listItem : Parser () -> Parser (Block Simple)
listItem mark = do
    mark
    line <- manyTill inline eol
    pure $ Para Simple line

olist : Parser (Block Simple)
olist = do
    is <- some (listItem olMarker)
    eol
    pure $ ListBlock NumberTy is

blist : Parser (Block Simple)
blist = do
    is <- some (listItem ulMarker)
    eol
    pure $ ListBlock BulletTy is

dlist : Parser (Block Simple)
dlist = do
    is <- some defItem <$ eol
    pure $ DList Simple is
  <?> "Description Lists"
  where
    marker : Parser (List (Inline Simple))
    marker = ulMarker $> space $> manyTill inline (space $> colon $> colon)

    defItem : Parser (List (Inline Simple), List (Block Simple))
    defItem = do
        key <- marker
        space
        values <- manyTill inline eol
        pure (key, [Para Simple values])


list : Parser (Block Simple)
list = dlist <|> blist <|> olist

orgBlock : Parser (Block Simple)
orgBlock = header <|> block <|> list <|> figure <|> para

parseOrg : Parser (Edda Simple)
parseOrg = do
  title  <- attribute "TITLE"
  author <- attribute "AUTHOR"
  date   <- attribute "DATE"
  txt    <- many orgBlock
  lpara  <- many paraLast -- Dirty Hack
  let ps = the Attributes [title, author, date]
  let txt' = intersperse (Empty Simple) txt
  pure $ MkEddaSimple (Just ps) (txt' ++ [Empty Simple] ++ lpara)
 <?> "Raw Org Mode"

-- -------------------------------------------------------------------- [ Read ]
readOrg : String -> {[FILE_IO ()]} Eff (Either String EddaDoc)
readOrg = readEddaFile parseOrg
