module Edda.Reader.Org

import public Control.Monad.Identity
import public Lightyear.Core
import public Lightyear.Combinators
import public Lightyear.Strings

import Edda.Effs
import Edda.Model
import Edda.Utils
import Edda.Reader.Common
import Edda.Reader.Utils

%access public

-- --------------------------------------------------------------------- [ Org ]

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
    txt <- brackets' $ brackets' url
    pure $ Link ExposedTy txt Nothing
  <?> "Exposed Link"

hyper : Parser (Inline Star)
hyper = do
    (uri, desc) <- brackets' internal
    pure $ Link HyperTy uri (Just desc)
  where
    internal : Parser (String, List (Inline Star))
    internal = do
      u <- brackets' url
      d <- brackets' $ some (text <$ space)
      pure (u, intersperse (Punc ' ' ) d)

link : Parser (Inline Star)
link = hyper <|> expLink <?> "Link"

fnote : Parser (Inline Star)
fnote = do
   (l,d) <- brackets' doFnote
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

target : Parser String
target = angles $ angles url
  <?> "Target"

-- ------------------------------------------------------------------ [ Blocks ]

getOrgBlockType : String -> Either VerbBlockTy TextBlockTy
getOrgBlockType str = case str of
    "COMMENT"     => Left CommentTy
    "SRC"         => Left ListingTy
    "EXAMPLE"     => Left LiteralTy
    "EQUATION"    => Left EquationTy
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

hrule : Parser (Block Star)
hrule = do
    token "-----"
    pure (HRule Star)

header : Parser (Block Star)
header = char '*' >! do
    depth <- opt (many $ char '*')
    space
    title <- manyTill (inline) (eol)
    l <- opt target
    space
    let d = length (fromMaybe [] depth) + 1
    pure (Header Star d l title)

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
listItem : Parser () -> Parser (List (Inline Star))
listItem mark = do
    mark
    line <- manyTill inline eol
    pure $ line

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

    defItem : Parser (List (Inline Star), List (Inline Star))
    defItem = do
        key <- marker
        space
        values <- manyTill inline eol
        pure (key, values)

list : Parser (Block Star)
list = dlist <|> blist <|> olist

orgBlock : Parser (Block Star)
orgBlock = header <|> block <|> list <|> figure <|> hrule <|> para

parseOrg : Parser (Edda Star)
parseOrg = do
  title  <- attribute "TITLE"
  author <- attribute "AUTHOR"
  date   <- attribute "DATE" <$ space
  txt    <- many orgBlock
  lpara  <- many paraLast -- Dirty Hack
  let ps = the Attributes [title, author, date]
  let txt' = intersperse (Empty Star) txt
  pure $ MkEddaRaw (Just ps) (txt' ++ [Empty Star] ++ lpara)
 <?> "Raw Org Mode"

-- -------------------------------------------------------------------- [ Read ]
public
readOrg : String -> {[FILE_IO ()]} Eff (Either String EddaDoc)
readOrg = readEddaFile parseOrg
