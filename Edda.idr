module Edda

data QType = SQuote | DQuote

Target : Type
Target = (String, String)

Attributes : Type
Attributes = List (String, String)

-- Dealing with citations
data CiteType = ParenCite | TextualCite

data Inline : Type where
  Serif : String -> Inline
  SmallCaps : List Inline -> Inline
  SnsSerif : List Inline -> Inline
  MonoSpace : List Inline -> Inline
  Emph : List Inline -> Inline
  Strong : List Inline -> Inline
  Strike : List Inline -> Inline
  SuperScript : List Inline -> Inline
  SubScript : List Inline -> Inline
  InlineQuote : (ty : QType) -> List Inline -> Inline
  CodeSnippet : Attributes -> String -> Inline
  MathSnippet : String -> Inline
  Link : (url : String) -> (title : String) -> List Inline -> Inline
  Citation : CiteType -> String -> (pfix : List Inline) -> (sfix : List Inline) -> Inline
  Space : Inline
  Linebreak : Inline

Sentance : Type
Sentance = List Inline

data TAlign = AlingLeft | AlignRight | AlignCenter | AlignPar

data Image : Attributes -> (alt : Sentance) -> Type where
  MkImage : (as : Attributes) -> (alt : Sentance) -> String -> Image as alt

data SecDepth = Section | SubSection | SubSubSection | Paragraph | Subparagraph
data TheoremTy = Normal | Corollary | Lemma | Proposition | Proof | Definition | Example | Exercises | Note | Problem | Question | Remark | Solution

mutual
  data Table : (rows : Nat) -> (cols : Nat) -> Type where
    MkTable : (align : Vect n TAlign) -> Vect m (Vect n (List (Block l c))) -> Table m n

  data Theorem : TheoremTy -> Maybe Sentance -> Type where
    MkTheroem : (ty : TheoremTy) -> (title : Maybe Sentance) -> List (Block l c) -> Theorem ty title

  -- Have label, title, as type values
  data Block : (label : Maybe String) -> (title : Maybe Sentance) -> Type where
    Plain         : Sentance -> Block Nothing Nothing
    CodeBlock     : (label : Maybe String) -> (caption : Maybe Sentance) -> Attributes -> String -> Block label caption
    EquationBlock : (label : Maybe String) -> (caption : Maybe Sentance) -> String -> Block label caption
    BQuote        : (label : Maybe String) -> Target -> List (Block lab cap) -> Block label Nothing

    OList         : (label : Maybe String) -> List (List (Block lab cap)) -> Block label Nothing
    BList         : (label : Maybe String) -> List (List (Block lab cap)) -> Block label Nothing
    DList         : (label : Maybe String) -> List (Inline, List (List (Block lab cap))) -> Block label Nothing

    FigureBlock   : (label : Maybe String) -> (caption : Maybe Sentance) -> (Image as alt) -> Block label caption
    TableBlock    : (label : Maybe String) -> (caption : Maybe Sentance) -> (Table m n) -> Block label caption
    TheoremBlock  : (label : Maybe String) -> (caption : Maybe Sentance) -> Theorem t caption -> Block label caption

    Heading       : (lvl : Nat) -> (label : Maybe String) -> (caption : Maybe Sentance) -> List (Block l c) -> Block label caption

    Empty         : Block Nothing Nothing

{-data Section : String -> Type where
  MkSection : (label : String) -> (title : Inline) -> List Block -> Maybe (Section ?a tit contents (Maybe c)) -> Section label
-}


data Edda : Maybe Attributes -> Type where
  MkEdda : (as : Maybe Attributes) -> List (Block l c) -> Edda as
