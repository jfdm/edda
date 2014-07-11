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
  MkImage : Maybe Attributes -> String -> Inline -> Image

instance Show Image where
  show (MkImage ats alt url) = "[" ++ concatMap (\(x, y) => unwords ["(", x, ",", y, ")"]) (fromMaybe [("no", "attributes")] ats) ++ " " ++ show alt ++ " " ++ show url ++ "]"

data TheoremTy = Normal
               | Corollary
               | Lemma
               | Proposition
               | Proof
               | Definition
               | Example
               | Exercise
               | Note
               | Problem
               | Question
               | Remark
               | Solution

instance Show TheoremTy where
  show Normal      = "THEOREM"
  show Corollary   = "COROLLARY"
  show Lemma       = "LEMMA"
  show Proposition = "PROPOSITION"
  show Proof       = "PROOF"
  show Definition  = "DEFINITION"
  show Example     = "EXAMPLE"
  show Exercise   = "EXERCISE"
  show Note        = "NOTE"
  show Problem     = "PROBLEM"
  show Question    = "QUESTION"
  show Remark      = "REMARK"
  show Solution    = "SOLUTION"

mutual
  data Tabular : Type where
    MkTabular : (align : Vect n TAlign) -> Vect m (Vect n (List Block)) -> Tabular

  data Block : Type where
    Para : Sentance -> Block

    Generic   : (label : Maybe String) -> (caption : Maybe Sentance) -> Maybe Attributes -> String -> Block
    Listing   : (label : Maybe String) -> (caption : Maybe Sentance) -> Maybe Attributes -> String -> Block
    Equation  : (label : Maybe String) -> String -> Block
    Quotation : List Inline -> Block

    OList : List Block -> Block
    BList : List Block -> Block
    DList : List (Sentance, List Block) -> Block

    Figure   : (label : String) -> (caption : Sentance) -> Image -> Block
    Table    : (label : String) -> (caption : Sentance) -> Tabular -> Block
    Theorem  : (label : Maybe String) -> (caption : Maybe Sentance) -> TheoremTy -> List Inline -> Block

    Heading : (lvl : Nat) -> (label : String) -> (title : Sentance) -> Block
    Empty   : Block


instance Show Tabular where
  -- incomplete
  show (MkTabular as cells) = "[Table " ++ "[" ++ concatMap (\x => show x ++ " ") as ++ "]" ++ " cells ]"

instance Show Block where
  show (Theorem l c ty thm) = "[ThmBlock " ++ show ty ++ fromMaybe "" l ++ " " ++ show c ++ " " ++ concatMap show thm ++ "]"
  show (Table l c tbl)   = "[TblBlock " ++ show l ++ " " ++ show c ++ " " ++ show tbl ++ "]"
  show (Figure l c img)  = "[FigBlock " ++ show l ++ " " ++ show c ++ " " ++ show img ++ "]"

  show (DList ds) = "[DList " ++ concatMap (\(k,vs) => show k ++ " " ++ concatMap show vs ) ds ++ "]"
  show (BList is) = "[BList " ++ concatMap show is ++ "]"
  show (OList is) = "[OList " ++ concatMap show is ++ "]"

  show (Quotation p)   = "[BQuote " ++ concatMap show p ++ "]"
  show (Equation l m)  = "[EqBlock " ++ fromMaybe "" l ++ " \"" ++ m ++ "\"]"
  -- Incomplete
  show (Listing l cap as co) = "[CodeBlock " ++ (fromMaybe "" l) ++ show cap ++ " \"" ++ co ++ "\"]"
  show (Generic l cap as co) = "[Generic " ++ (fromMaybe "" l) ++ " " ++ show cap ++ " \""

  show (Heading d l t) = "[Heading " ++ show d ++ " " ++ show l ++ " " ++ show t ++ "]"
  show (Para p) = "[Para " ++ concatMap show p ++ "]"
  show Empty = "[Empty]"

Property : Type
Property = (String, String)

Properties : Type
Properties = List Property

data Edda : Type where
  MkEdda : (as : Maybe Properties) -> List (Block) -> Edda

instance Show Edda where
  -- incomplete
  show (MkEdda ps body) = "[Edda " ++ "[Mdata " ++ concatMap show ps ++ "]" ++ concatMap show body ++ "]"
