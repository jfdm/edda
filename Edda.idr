module Edda

data QType = SQuote | DQuote

Target : Type
Target = (String, String)

data CodeAttr : Type where
  MkCodeAttr : (id : String) -> List String -> List (String, String) -> CodeAttr

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
  Quote : (ty : QType) -> List Inline -> Inline
  Space : Inline
  CodeSnippet : CodeAttr -> String -> Inline
  Linebreak : Inline
  MathSnippet : String -> Inline
  Link : (url : String) -> (title : String) -> List Inline -> Inline
  Image : (url : String) -> (alt : String) -> Inline
  Citation : CiteType -> String -> (pfix : List Inline) -> (sfix : List Inline) -> Inline


data TAlign = AlingLeft | AlignRight | AlignCenter | AlignDefault

data TheoremTy = Corollary | Lemma | Proposition | Theorem | Proof | Definition | Example | Exercises | Note | Problem | QUestion | Remark | Solution

-- Have label, title, as type values
data Block : Type where
  Plain : List Inline -> Block
  Para  : List Inline -> Block
  CodeBlock  : (caption : Inline) -> (label : String) -> CodeAttr -> String -> Block
  MathBlock : String -> Block
  BQuote : List Block -> Block
  OList : List (List Block) -> Block
  BList : List (List Block) -> Block
  DList : List (Inline, List (List Block)) -> Block
  FigureBlock : List Inline -> String -> Inline -> Block
  TableBlock : (caption : List Inline) -> (alignments : Vect n TAlign) -> Vect m (Vect n (List Block)) -> Block
  TheoremBlock : (label : String) -> TheoremTy -> (title : List Inline) -> List Block -> Block
  Heading : String -> List Inline -> List Inline -> Block
  Null  : Block

{-data Section : String -> Type where
  MkSection : (label : String) -> (title : Inline) -> List Block -> Maybe (Section ?a tit contents (Maybe c)) -> Section label
-}
data MData : Type where
  MkMData : String -> String -> String -> MData

data Edda : MData -> Type where
  MkEdda : (md : MData) -> List Block -> Edda md
