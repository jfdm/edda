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

text : Parser (Inline Star)
text = map (Font SerifTy) word <?> "Raw Word"

punc : Parser (Inline Star)
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

mText : Parser (Inline Star)
mText = text <|> map Punc borderPunc <?> "Texted used in markup"

code : Parser (Inline Star)
code = map (Raw CodeTy) (literallyBetween '~') <?> "Code"

verb : Parser (Inline Star)
verb = map (Raw VerbTy) (literallyBetween '=') <?> "Verb"

math : Parser (Inline Star)
math = map (Raw MathTy) (literallyBetween '$') <?> "Math"

markup : MarkupTy -> Char -> Parser (Inline Star)
markup mTy c = do
    txt <- between (char c) (char c) (some mText)
    pure $ Mark mTy txt
  <?> "Markup"

bold : Parser (Inline Star)
bold = markup BoldTy '*' <?> "Bold"

emph : Parser (Inline Star)
emph = markup EmphTy '/'  <?> "Emph"

strike : Parser (Inline Star)
strike = markup StrikeTy '+' <?> "Strike"

uline : Parser (Inline Star)
uline = markup UlineTy '_' <?> "Uline"

expLink : Parser (Inline Star)
expLink = do
    txt <- brackets $ brackets url
    pure $ Link ExposedTy txt Nothing
  <?> "Exposed Link"

hyper : Parser (Inline Star)
hyper = do
    (uri, desc) <- brackets internal
    pure $ Link HyperTy uri (Just desc)
  where
    internal : Parser (String, List (Inline Star))
    internal = do
      u <- brackets url
      d <- brackets $ some text
      pure (u, intersperse (Punc ' ' ) d)

link : Parser (Inline Star)
link = hyper <|> expLink <?> "Link"

fnote : Parser (Inline Star)
fnote = do
   (l,d) <- brackets doFnote
   pure $ Link FnoteTy (fromMaybe "" l) d
 where
   doFnote : Parser (Maybe String, Maybe (List (Inline Star)))
   doFnote = do
     string "fn"
     colon
     lab <- opt word
     colon
     desc <- opt $ some text
     pure (lab, desc)

inline : Parser (Inline Star)
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

inlineKeyWord : String -> Parser (String, List (Inline Star))
inlineKeyWord key = do
    string "#+" $> string key
    colon
    space
    ps <- manyTill (inline) eol
    pure (key, ps)
  <?> "Attribute."

caption : Parser (List (Inline Star))
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
-- @TODO Add equations

getOrgBlockType : String -> Either VerbBlockTy TextBlockTy
getOrgBlockType str = case str of
    "COMMENT"     => Left CommentTy
    "SRC"         => Left ListingTy
    "EXAMPLE"     => Left LiteralTy
    "QUOTE"       => Right QuotationTy
    "VERSE"       => Right QuotationTy
    "THEOREM"     => Right TheoremTy
    "COROLLARY"   => Right CorollaryTy
    "LEMMA"       => Right LemmaTy
    "PROPOSITION" => Right PropositionTy
    "PROOF"       => Right ProofTy
    "DEFINITION"  => Right DefinitionTy
    "EXERCISE"    => Right ExerciseTy
    "NOTE"        => Right NoteTy
    "PROBLEM"     => Right ProblemTy
    "QUESTION"    => Right QuestionTy
    "REMARK"      => Right RemarkTy
    "SOLUTION"    => Right SolutionTy
    otherwise     => Left LiteralTy

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


block : Parser (Block Star)
block = do
    cap  <- opt caption
    lab  <- opt label
    attr <- opt $ attribute "ATTR"
    string "#+BEGIN_"
    ty <- word
    case getOrgBlockType ty of
      Left x => do
        srcopts <- opt $ char ' ' $> manyTill (anyChar) eol
        let as = dealWithSrcAttrs (convertOpts srcopts) (convertAttrs attr)

        txt <- manyTill anyChar (string "#+END_" $> token ty)
        pure $ VerbBlock x lab cap as (pack txt)
      Right y => do
        txt <- manyTill inline (string "#+END_" $> token ty)
        pure $ TextBlock y lab cap (convertAttrs attr) txt

   <?> "Blocks"

figure : Parser (Block Star)
figure = do
    cap <- caption
    lab <- label
    as  <- opt $ some (attribute "ATTR")
    img <- expLink
    space
    pure (Figure Star lab cap as img)
  <?> "Figure"

para : Parser (Block Star)
para = do
    txt <- manyTill inline (eol $> eol)
    space
    pure $ TextBlock ParaTy Nothing Nothing Nothing txt
  <?> "Paragraphs"

paraLast : Parser (Block Star)
paraLast = do
    txt <- manyTill inline (eol $> space)
    pure $ TextBlock ParaTy Nothing Nothing Nothing txt
  <?> "Filthy hack for last para"

header : Parser (Block Star)
header = char '*' >! do
    depth <- opt (many $ char '*')
    space
    title <- manyTill (inline) (eol $> space)
    let d = length (fromMaybe [] depth) + 1
    pure (Header Star d "" title)


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
listItem : Parser () -> Parser (Block Star)
listItem mark = do
    mark
    line <- manyTill inline eol
    pure $ TextBlock ParaTy Nothing Nothing Nothing line

olist : Parser (Block Star)
olist = do
    is <- some (listItem olMarker)
    eol
    pure $ ListBlock NumberTy is

blist : Parser (Block Star)
blist = do
    is <- some (listItem ulMarker)
    eol
    pure $ ListBlock BulletTy is

dlist : Parser (Block Star)
dlist = do
    is <- some defItem <$ eol
    pure $ DList Star is
  <?> "Description Lists"
  where
    marker : Parser (List (Inline Star))
    marker = ulMarker $> space $> manyTill inline (space $> colon $> colon)

    defItem : Parser (List (Inline Star), List (Block Star))
    defItem = do
        key <- marker
        space
        values <- manyTill inline eol
        pure (key, [TextBlock ParaTy Nothing Nothing Nothing values])


list : Parser (Block Star)
list = dlist <|> blist <|> olist

orgBlock : Parser (Block Star)
orgBlock = header <|> block <|> list <|> figure <|> para

parseOrg : Parser (Edda Star)
parseOrg = do
  title  <- attribute "TITLE"
  author <- attribute "AUTHOR"
  date   <- attribute "DATE"
  txt    <- many orgBlock
  lpara  <- many paraLast -- Dirty Hack
  let ps = the Attributes [title, author, date]
  let txt' = intersperse (Empty Star) txt
  pure $ MkEddaStar (Just ps) (txt' ++ [Empty Star] ++ lpara)
 <?> "Raw Org Mode"

-- -------------------------------------------------------------------- [ Read ]
readOrg : String -> {[FILE_IO ()]} Eff (Either String EddaDoc)
readOrg = readEddaFile parseOrg
