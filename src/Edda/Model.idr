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

data ParenTy = Parents | Brackets | Braces

instance Show ParenTy where
  show Parents = "Parens"
  show Brackets = "Brackets"
  show Braces = "Braces"

Attributes : Type
Attributes = List (String, String)

mutual

  data Inline : Type where
    Serif     : String -> Inline
    SmallCaps : List Inline -> Inline
    SansSerif : List Inline -> Inline

    Emph   : List Inline -> Inline
    Strong : List Inline -> Inline
    Strike : List Inline -> Inline
    Uline  : List Inline -> Inline

    Quote  : QuoteTy -> List Inline -> Inline
    Parens : ParenTy -> List Inline -> Inline

    Verbatim    : String -> Inline
    CodeSnippet : String -> Inline
    MathSnippet : String -> Inline

    Link  : LinkTy -> String -> List Inline -> Inline
    Cite  : CiteTy -> String -> Inline
    FNote : String -> List Inline -> Inline

instance Show Inline where
  show (Serif s) = "{Text \"" ++ s ++ "\"}"
  show (SmallCaps ss) = "{SmallCaps " ++ show ss ++ "}"
  show (SansSerif ss) = "{SansSerif " ++ show ss ++ "}"

  show (Emph ss)   = "{Emph "   ++ show ss ++ "}"
  show (Strong ss) = "{Strong " ++ show ss ++ "}"
  show (Strike ss) = "{Strike " ++ show ss ++ "}"
  show (Uline ss)  = "{Verb "    ++ show ss ++ "}"

  show (Quote qty ss) = "{" ++ show qty ++ " " ++ show ss ++ "}"
  show (Parens pty ss) = "{" ++ show pty ++ " " ++ show ss ++ "}"

  show (Verbatim verb)    = "{Verb \"" ++ verb ++ "\"}"
  show (CodeSnippet code) = "{Code \"" ++ code ++ "\"}"
  show (MathSnippet math) = "{Math \"" ++ math ++ "\"}"

  show (Link ty u t) = "{Link " ++ show ty ++  " <" ++ u ++ "> " ++ show t ++ "}"
  show (Cite ty id) = "{" ++ show ty ++ " " ++ id ++ "}"
  show (FNote label desc) = "{FNote \"" ++ label ++ "\" \"" ++ show desc ++ "\"}"

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
  show (MkTabular as cells) = "[Table " ++ "[" ++ concatMap (\x => show x ++ " ") as ++ "]" ++ " cells ]\n"

instance Show Block where
  show (Theorem l c ty thm) = "[ThmBlock " ++ show ty ++ " " ++ fromMaybe "" l ++ " " ++ show c ++ " " ++ concatMap show thm ++ "]\n"
  show (Table l c tbl)   = "[TblBlock " ++ show l ++ " " ++ show c ++ " " ++ show tbl ++ "]\n"
  show (Figure l c img)  = "[FigBlock " ++ show l ++ " " ++ show c ++ " " ++ show img ++ "]\n"

  show (DList ds) = "[DList " ++ concatMap (\(k,vs) => show k ++ " " ++ concatMap show vs ) ds ++ "]\n"
  show (BList is) = "[BList " ++ concatMap show is ++ "]\n"
  show (OList is) = "[OList " ++ concatMap show is ++ "]\n"

  show (Quotation p)   = "[BQuote " ++ concatMap show p ++ "]\n"
  show (Equation l m)  = "[EqBlock " ++ fromMaybe "" l ++ " \"" ++ m ++ "\"]\n"
  -- Incomplete
  show (Listing l cap as co) = "[CodeBlock " ++ (fromMaybe "" l) ++ show cap ++ " \"" ++ co ++ "\"]\n"
  show (Generic l cap as co) = "[Generic " ++ (fromMaybe "" l) ++ " " ++ show cap ++ " \"]\n"

  show (Heading d l t) = "[Heading " ++ show d ++ " " ++ show l ++ " " ++ show t ++ "]\n"
  show (Para p) = "[Para " ++ concatMap show p ++ "]\n"
  show Empty = "[Empty]\n"

Property : Type
Property = (String, String)

Properties : Type
Properties = List Property

data Edda : Type where
  MkEdda : (as : Maybe Properties) -> List (Block) -> Edda

instance Show Edda where
  -- incomplete
  show (MkEdda ps body) = "[Edda " ++ "[Mdata " ++ concatMap show ps ++ "]\n" ++ concatMap show body ++ "]\n"
