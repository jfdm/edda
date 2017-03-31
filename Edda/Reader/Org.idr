-- ----------------------------------------------------------------- [ Org.idr ]
-- Module    : Org.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Edda.Reader.Org

import Effects
import Effect.File

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

import Edda.Model
import Edda.Utils
import Edda.Reader.Common
import Edda.Reader.Utils

import public Edda.Refine

%access private

-- --------------------------------------------------------------------- [ Org ]

code : Parser (Edda STAR INLINE)
code = map (Raw CodeTy) (quoted '~') <?> "Code"

verb : Parser (Edda STAR INLINE)
verb = map (Raw VerbTy) (quoted '=') <?> "Verb"

math : Parser (Edda STAR INLINE)
math = map (Raw MathTy) (quoted '$') <?> "Math"

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
      d <- brackets $ some (text <* spaces)
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

export
inline : Parser (Edda STAR INLINE)
inline = text
     <|> fnote <|> link
     <|> bold <|> emph <|> strike <|> uline
     <|> code <|> verb <|> math <|> punc
     <?> "Raw Inline"

-- -------------------------------------------------------------- [ Properties ]

export
attribute : String -> Parser (String, String)
attribute key = do
    string "#+" *> string key
    colon
    ps <- manyTill anyChar endOfLine
    pure (key, pack ps)
  <?> "Raw Attribute"

property : Parser (String, String)
property = attribute "PROPERTY" <?> "Property"

inlineKeyWord : String -> Parser (String, List (Edda STAR INLINE))
inlineKeyWord key = do
    string "#+" *> string key
    colon
    spaces
    ps <- manyTill inline endOfLine
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
    spaces
    value <- manyTill anyChar endOfLine
    pure (key, pack value)
  <?> "Property Entry"

drawer : Parser $ List (String, String)
drawer = do
    string ":PROPERTIES:"
    endOfLine
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

export
block : Parser (Edda STAR BLOCK)
block = do
    cap  <- opt caption
    lab  <- opt label
    attr <- opt $ attribute "ATTR"
    string "#+BEGIN_"
    ty <- word
    case getOrgBlockType ty of
      Left x => do
        srcopts <- opt $ space  *> manyTill anyChar endOfLine
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
    spaces
    pure (Figure STAR lab cap (fromMaybe Nil as) img)
  <?> "Figure"

export
para : Parser (Edda STAR BLOCK)
para = do
    txt <- manyTill inline (endOfLine *> endOfLine)
    spaces
    pure $ TextBlock ParaTy Nothing Nil Nil txt
  <?> "Paragraphs"

paraLast : Parser (Edda STAR BLOCK)
paraLast = do
    txt <- manyTill inline (endOfLine *> spaces)
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
      some $ digit
      char c
      spaces
      pure ()

-- @TODO Add coninuations
listItem : Parser () -> Parser (List (Edda STAR INLINE))
listItem mark = do
    mark
    line <- manyTill inline endOfLine
    pure $ line

olist : Parser (Edda STAR BLOCK)
olist = do
    is <- some (listItem olMarker)
    endOfLine
    pure $ ListBlock NumberTy is

blist : Parser (Edda STAR BLOCK)
blist = do
    is <- some (listItem ulMarker)
    endOfLine
    pure $ ListBlock BulletTy is
  <?> "Bulleted lists"

dlist : Parser (Edda STAR BLOCK)
dlist = do
    is <- some defItem <* endOfLine
    pure $ DList STAR is
  <?> "Description Lists"
  where
    marker : Parser (List (Edda STAR INLINE))
    marker = ulMarker *> space *> manyTill inline (spaces *> colon *> colon)
        <?> "Desc Marker"

    defItem : Parser (List (Edda STAR INLINE), List (Edda STAR INLINE))
    defItem = do
        key <- marker
        spaces
        values <- manyTill inline endOfLine
        pure (key, values)
      <?> "Desc Lists"

list : Parser (Edda STAR BLOCK)
list = dlist <|> blist <|> olist <?> "Lists"

export
header : Parser (Edda STAR BLOCK)
header = char '*' >! do
    depth <- opt (many $ char '*')
    let d = length (fromMaybe [] depth) + 1
    spaces
    title <- manyTill (inline) (endOfLine)
    l <- opt target
    spaces
    as <- opt drawer
    pure $ Section STAR d l title (fromMaybe Nil as)

export
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
export
readOrg : String -> Eff (Either String (Edda PRIME MODEL)) [FILE ()]
readOrg = readEddaFile parseOrg

export
readOrgInline : String -> Either String EddaString
readOrgInline = readEddaSentance inline

export
readOrgBody : String -> Either String EddaBody
readOrgBody s = readEddaBody orgBlock (s ++ "\n\n")

-- --------------------------------------------------------------------- [ EOF ]
