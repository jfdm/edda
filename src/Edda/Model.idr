module Edda.Model

data QuoteTy = SQuote | DQuote

instance Show QuoteTy where
  show SQuote = "SQuote"
  show DQuote = "DQuote"

data CiteTy = ParenCite | TextCite

instance Show CiteTy where
  show ParenCite = "ParenCite"
  show TextCite  = "TextCite"

data LinkTy = ExLink | InLink

instance Show LinkTy where
  show ExLink = "External"
  show InLink = "Internal"

Attributes : Type
Attributes = List (String, String)

mutual

  data Inline : Type where
    Serif     : String -> Inline
    SmallCaps : Inline -> Inline
    SansSerif : Inline -> Inline
    MonoSpace : Inline -> Inline

    Emph   : Inline -> Inline
    Strong : Inline -> Inline
    Strike : Inline -> Inline
    Verb   : Inline -> Inline

    Quote : QuoteTy -> Inline -> Inline

    CodeSnippet : String -> Inline
    MathSnippet : String -> Inline

    Link : LinkTy -> String -> Inline -> Inline
    Cite : CiteTy -> String -> Inline

instance Show Inline where
  show (Serif s) = "{" ++ s ++ "}"
  show (SmallCaps ss) = "{SmallCaps " ++ show ss ++ "}"
  show (SansSerif ss) = "{SansSerif " ++ show ss ++ "}"
  show (MonoSpace ss) = "{MonoSpace " ++ show ss ++ "}"

  show (Emph ss)      = "{Emph "   ++ show ss ++ "}"
  show (Strong ss)    = "{Strong " ++ show ss ++ "}"
  show (Strike ss)    = "{Strike " ++ show ss ++ "}"
  show (Verb ss)      = "{Verb"    ++ show ss ++ "}"

  show (Quote qty ss) = "{" ++ show qty ++ " " ++ show ss ++ "}"

  show (CodeSnippet code) = "{Code \"" ++ code ++ "\"}"
  show (MathSnippet math)    = "{Math \"" ++ math ++ "\"}"

  show (Link ty u t) = "{Link " ++ show ty ++  "<" ++ u ++ "> " ++ show t ++ "}"

  show (Cite ty id) = "{" ++ show ty ++ " " ++ id ++ "}"


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

    Heading       : (lvl : Nat) -> (label : String) -> (title : Sentance) -> Block
    Empty         : Block

instance Show Table where
  -- incomplete
  show (MkTable as cells) = "[Table " ++ "[" ++ concatMap (\x => show x ++ " ") as ++ "]" ++ " cells ]"

instance Show Block where
  show Empty = "[Empty]"
  show (Heading d l t) = "[Heading " ++ show d ++ " " ++ show l ++ " " ++ show t ++ "]"

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

Property : Type
Property = (String, String)

Properties : Type
Properties = List Property

data Edda : Type where
  MkEdda : (as : Maybe Properties) -> List (Block) -> Edda

instance Show Edda where
  -- incomplete
  show (MkEdda ps body) = "[Edda " ++ "[Mdata " ++ concatMap show ps ++ "]" ++ concatMap show body ++ "]"
