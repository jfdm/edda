-- ----------------------------------------------------------------- [ Org.idr ]
-- Module    : Org.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Edda.Reader.Org

import public Lightyear
import public Lightyear.Strings

import Edda.Effs
import Edda.Model
import Edda.Utils
import Edda.Reader.Common
import Edda.Reader.Utils

import public Edda.Refine

%access public

-- --------------------------------------------------------------------- [ Org ]

code : Parser (Edda STAR INLINE)
code = map (Raw CodeTy) (literallyBetween '~') <?> "Code"

verb : Parser (Edda STAR INLINE)
verb = map (Raw VerbTy) (literallyBetween '=') <?> "Verb"

math : Parser (Edda STAR INLINE)
math = map (Raw MathTy) (literallyBetween '$') <?> "Math"

markup : MarkupTy -> Char -> Parser (Edda STAR INLINE)
markup mTy c = do
    txt <- between (char c) (char c) (some mText)
    pure $ Mark mTy txt
  <?> "Markup"

bold : Parser (Edda STAR INLINE)
bold = markup BoldTy '*' <?> "Bold"

emph : Parser (Edda STAR INLINE)
emph = markup EmphTy '/'  <?> "Emph"

strike : Parser (Edda STAR INLINE)
strike = markup StrikeTy '+' <?> "Strike"

uline : Parser (Edda STAR INLINE)
uline = markup UlineTy '_' <?> "Uline"

expLink : Parser (Edda STAR INLINE)
expLink = do
    txt <- brackets $ brackets url
    pure $ Link ExposedTy txt Nil
  <?> "Exposed Link"

hyper : Parser (Edda STAR INLINE)
hyper = do
    (uri, desc) <- brackets internal
    pure $ Link HyperTy uri desc
  where
    internal : Parser (String, List (Edda STAR INLINE))
    internal = do
      u <- brackets url
      d <- brackets $ some (text <* space)
      pure (u, intersperse (Punc ' ' ) d)

link : Parser (Edda STAR INLINE)
link = hyper <|> expLink <?> "Link"

fnote : Parser (Edda STAR INLINE)
fnote = do
   (l,d) <- brackets doFnote
   pure $ Link FnoteTy (fromMaybe "" l) d
 where
   doFnote : Parser (Maybe String, (List (Edda STAR INLINE)))
   doFnote = do
     string "fn"
     colon
     lab <- opt word
     colon
     desc <- opt $ some text
     pure (lab, fromMaybe Nil desc)

inline : Parser (Edda STAR INLINE)
inline = text
     <|> fnote <|> link
     <|> bold <|> emph <|> strike <|> uline
     <|> code <|> verb <|> math <|> punc
     <?> "Raw Inline"

-- -------------------------------------------------------------- [ Properties ]
attribute : String -> Parser (String, String)
attribute key = do
    string "#+" *> string key
    colon
    ps <- manyTill (anyChar) eol
    pure (key, pack ps)
  <?> "Raw Attribute"

property : Parser (String, String)
property = attribute "PROPERTY" <?> "Property"

inlineKeyWord : String -> Parser (String, List (Edda STAR INLINE))
inlineKeyWord key = do
    string "#+" *> string key
    colon
    space
    ps <- manyTill (inline) eol
    pure (key, ps)
  <?> "Attribute."

caption : Parser (List (Edda STAR INLINE))
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

-- ----------------------------------------------------------------- [ Drawers ]

propEntry : Parser (String, String)
propEntry = do
    colon
    key <- word
    colon
    space
    value <- manyTill (anyChar) eol
    pure (key, pack value)
  <?> "Property Entry"

drawer : Parser $ List (String, String)
drawer = do
    string ":PROPERTIES:"
    eol
    ps <- some propEntry
    token ":END:"
    pure ps
  <?> "Property Drawer"
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

block : Parser (Edda STAR BLOCK)
block = do
    cap  <- opt caption
    lab  <- opt label
    attr <- opt $ attribute "ATTR"
    string "#+BEGIN_"
    ty <- word
    case getOrgBlockType ty of
      Left x => do
        srcopts <- opt $ char ' ' *> manyTill (anyChar) eol
        let as = dealWithSrcAttrs (convertOpts srcopts) (convertAttrs attr)

        txt <- manyTill anyChar (string "#+END_" *> token ty)
        pure $ VerbBlock x lab (fromMaybe Nil cap) as (pack txt)
      Right y => do
        txt <- manyTill inline (string "#+END_" *> token ty)
        pure $ TextBlock y lab (fromMaybe Nil cap) (convertAttrs attr) txt

   <?> "Blocks"

figure : Parser (Edda STAR BLOCK)
figure = do
    cap <- caption
    lab <- label
    as  <- opt $ some (attribute "ATTR")
    img <- expLink
    space
    pure (Figure STAR lab cap (fromMaybe Nil as) img)
  <?> "Figure"

para : Parser (Edda STAR BLOCK)
para = do
    txt <- manyTill inline (eol *> eol)
    space
    pure $ TextBlock ParaTy Nothing Nil Nil txt
  <?> "Paragraphs"

paraLast : Parser (Edda STAR BLOCK)
paraLast = do
    txt <- manyTill inline (eol *> space)
    pure $ TextBlock ParaTy Nothing Nil Nil txt
  <?> "Filthy hack for last para"

hrule : Parser (Edda STAR BLOCK)
hrule = do
    token "-----"
    pure (HRule STAR)

ulMarker : Parser ()
ulMarker = char' '+' <|> char' '-' <?> "UList Marker"

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
listItem : Parser () -> Parser (List (Edda STAR INLINE))
listItem mark = do
    mark
    line <- manyTill inline eol
    pure $ line

olist : Parser (Edda STAR BLOCK)
olist = do
    is <- some (listItem olMarker)
    eol
    pure $ ListBlock NumberTy is

blist : Parser (Edda STAR BLOCK)
blist = do
    is <- some (listItem ulMarker)
    eol
    pure $ ListBlock BulletTy is
  <?> "Bulleted lists"

dlist : Parser (Edda STAR BLOCK)
dlist = do
    is <- some defItem <* eol
    pure $ DList STAR is
  <?> "Description Lists"
  where
    marker : Parser (List (Edda STAR INLINE))
    marker = ulMarker *> space *> manyTill inline (space *> colon *> colon)
        <?> "Desc Marker"

    defItem : Parser (List (Edda STAR INLINE), List (Edda STAR INLINE))
    defItem = do
        key <- marker
        space
        values <- manyTill inline eol
        pure (key, values)
      <?> "Desc Lists"

list : Parser (Edda STAR BLOCK)
list = dlist <|> blist <|> olist <?> "Lists"

header : Parser (Edda STAR BLOCK)
header = char '*' >! do
    depth <- opt (many $ char '*')
    let d = length (fromMaybe [] depth) + 1
    space
    title <- manyTill (inline) (eol)
    l <- opt target
    space
    as <- opt drawer
    pure $ Section STAR d l title (fromMaybe Nil as)

orgBlock : Parser (Edda STAR BLOCK)
orgBlock = header <|> block <|> list <|> figure <|> hrule <|> para <?> "Org Blocks"

parseOrg : Parser (Edda STAR MODEL)
parseOrg = do
  title  <- attribute "TITLE"
  author <- attribute "AUTHOR"
  date   <- attribute "DATE" <* space
  txt    <- many orgBlock
  lpara  <- many paraLast -- Dirty Hack
  let ps = the (List (String, String)) [title, author, date]
  let txt' = intersperse (Empty STAR) txt
  pure $ EddaRaw (ps) (txt' ++ [Empty STAR] ++ lpara)
 <?> "Raw Org Mode"

-- -------------------------------------------------------------------- [ Read ]
public
readOrg : String -> {[FILE_IO ()]} Eff (Either String (Edda PRIME MODEL))
readOrg = readEddaFile parseOrg


-- --------------------------------------------------------------------- [ EOF ]
