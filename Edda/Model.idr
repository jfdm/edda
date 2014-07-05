module Edda.Model

data QuoteTy = SQuote | DQuote

instance Show QuoteTy where
  show SQuote = "SQuote"
  show DQuote = "DQuote"

data CiteTy = ParenCite | TextCite

instance Show CiteTy where
  show ParenCite = "ParenCite"
  show TextCite  = "TextCite"

Attributes : Type
Attributes = List (String, String)

mutual

  data Inline : Type where
    Serif     : String      -> Inline
    SmallCaps : List Inline -> Inline
    SansSerif  : List Inline -> Inline
    MonoSpace : List Inline -> Inline

    Emph   : List Inline -> Inline
    Strong : List Inline -> Inline
    Strike : List Inline -> Inline

    Quote : QuoteTy -> List Inline -> Inline

    CodeSnippet : Attributes -> String -> Inline

    MathSnippet : String -> Inline

    URL  : String -> Inline
    Link : String -> List Inline -> Inline
    Ref  : String -> Inline
    Cite : CiteTy -> String -> Inline

instance Show Inline where
  show (Serif s) = "{" ++ s ++ "}"
  show (SmallCaps ss) = "{SmallCaps " ++ show ss ++ "}"
  show (SansSerif ss) = "{SansSerif " ++ show ss ++ "}"
  show (MonoSpace ss) = "{MonoSpace " ++ show ss ++ "}"

  show (Emph ss)      = "{Emph "   ++ show ss ++ "}"
  show (Strong ss)    = "{Strong " ++ show ss ++ "}"
  show (Strike ss)    = "{Strike " ++ show ss ++ "}"

  show (Quote qty ss) = "{" ++ show qty ++ " " ++ show ss ++ "}"

  show (CodeSnippet as code) = "{Code \"" ++ code ++ "\"}"
  show (MathSnippet math)    = "{Math \"" ++ math ++ "\"}"

  show (URL u)    = "{Url "  ++ u ++ "}"
  show (Link u t) = "{Link " ++ "<" ++ u ++ "> " ++ show t ++ "}"
  show (Ref u)    = "{Ref "  ++ u ++ "}"

  show (Cite ty id) = "{" ++ show ty ++ id ++ "}"


Sentance : Type
Sentance = List Inline

data TAlign = AlignLeft
            | AlignRight
            | AlignCenter
            | AlignPar

instance Show TAlign where
  show AlignLeft = "l"
  show AlignRight = "r"
  show AlignCenter = "c"
  show AlignPar = "p"

data Image : Type where
  MkImage : Attributes -> String -> String -> Image

instance Show Image where
  show (MkImage ats alt url) = "[" ++ concatMap (\(x, y) => unwords ["(", x, ",", y, ")"]) ats ++ " " ++ show url ++ "]"

data TheoremTy = Theorem
               | Corollary
               | Lemma
               | Proposition
               | Proof
               | Definition
               | Example
               | Exercises
               | Note
               | Problem
               | Question
               | Remark
               | Solution

instance Show TheoremTy where
  show Theorem     = "Theorem"
  show Corollary   = "Corollary"
  show Lemma       = "Lemma"
  show Proposition = "Proposition"
  show Proof       = "Proof"
  show Definition  = "Definition"
  show Example     = "Example"
  show Exercises   = "Exercises"
  show Note        = "Note"
  show Problem     = "Problem"
  show Question    = "Question"
  show Remark      = "Remark"
  show Solution    = "Solution"

mutual
  data Table : Type where
    MkTable : (align : Vect n TAlign) -> Vect m (Vect n (List Block)) -> Table

  data Block : Type where
    Plain : Sentance -> Block

    CodeBlock  : (label : String) -> (caption : Sentance) -> Attributes -> String -> Block
    MathBlock  : (label : String) -> String -> Block
    QuoteBlock : List Block -> Block

    OList         : List Block -> Block
    BList         : List Block -> Block
    DList         : List (Sentance, List Block) -> Block

    FigureBlock   : (label : String) -> (caption : Sentance) -> Image -> Block
    TableBlock    : (label : String) -> (caption : Sentance) -> Table -> Block
    TheoremBlock  : (label : String) -> (caption : Sentance) -> TheoremTy -> List Block -> Block

    Heading       : (lvl : Nat) -> (label : String) -> (caption : Sentance) -> List Block  -> Block
    Empty         : Block

instance Show Table where
  -- incomplete
  show (MkTable as cells) = "[Table " ++ "[" ++ concatMap (\x => show x ++ " ") as ++ "]" ++ " cells ]"

instance Show Block where
  show Empty = "[Empty]"
  show (Heading d l c bs) = "[Heading " ++ show d ++ " " ++ show l ++ " " ++ concatMap show bs ++ "]"

  show (TheoremBlock l c ty thm) = "[ThmBlock " ++ show ty ++ show l ++ " " ++ show c ++ " " ++ concatMap show thm ++ "]"
  show (TableBlock l c tbl)   = "[TblBlock " ++ show l ++ " " ++ show c ++ " " ++ show tbl ++ "]"
  show (FigureBlock l c img)  = "[FigBlock " ++ show l ++ " " ++ show c ++ " " ++ show img ++ "]"

  show (DList ds) = "[DList " ++ concatMap (\(k,vs) => show k ++ " " ++ concatMap show vs ) ds ++ "]"
  show (BList is) = "[BList " ++ concatMap show is ++ "]"
  show (OList is) = "[OList " ++ concatMap show is ++ "]"

  show (QuoteBlock p)       = "[BQuote " ++ concatMap show p ++ "]"
  show (MathBlock l m)      = "[EqBlock " ++ show l ++ " \"" ++ m ++ "\"]"

  -- Incomplete
  show (CodeBlock l cap as co) = "[CodeBlock " ++ show l ++ " " ++ show cap ++ " \"" ++ co ++ "\"]"

  show (Plain p) = "[Plain " ++ concatMap show p ++ "]"

data PropType = Title
              | Author
              | Date

instance Show PropType where
  show Title  = "Title"
  show Author = "Author"
  show Date   = "Date"

Property : Type
Property = (PropType, String)

Properties : Type
Properties = List Property

data Edda : Type where
  MkEdda : (as : Maybe Properties) -> List (Block) -> Edda

instance Show Edda where
  -- incomplete
  show (MkEdda _ body) = "[Edda " ++ concatMap show body ++ "]"
