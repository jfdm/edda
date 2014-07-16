module Edda.Model

import Edda.Model.Utils

data Step = Raw | Dash | Prime

instance Show Step where
  show Raw   = "Raw"
  show Dash  = "Dash"
  show Prime = "Prime"

data Inline : Step -> Type where
  Text : (s : Step) -> String -> Inline s
  Punc : (s : Step) -> PuncTy -> Char -> Inline s
  Link : (s : Step) -> LinkTy -> String -> Maybe (List (Inline s)) -> Inline s

  Verbatim    : (s : Step) -> String -> Inline s
  CodeSnippet : (s : Step) -> String -> Inline s
  MathSnippet : (s : Step) -> String -> Inline s

  SmallCaps : List (Inline Prime) -> Inline Prime
  SansSerif : List (Inline Prime) -> Inline Prime

  Emph   : List (Inline Prime) -> Inline Prime
  Strong : List (Inline Prime) -> Inline Prime
  Strike : List (Inline Prime) -> Inline Prime
  Uline  : List (Inline Prime) -> Inline Prime

  Quote  : QuoteTy -> List (Inline Prime) -> Inline Prime
  Parens : ParenTy -> List (Inline Prime) -> Inline Prime

  Cite  : CiteTy -> String -> Inline Prime
  FNote : String -> List (Inline Prime) -> Inline Prime


instance Show (Inline ty) where
  show (Text s txt)   = "{Text " ++ show s ++ " \"" ++ txt ++ "\"}"
  show (Punc s pTy c) = "{Punc " ++ show s ++ " \""
       ++ case pTy of
            Other => "'" ++ show c ++ "'"
            _     => show pTy
       ++ "\"}"
  show (Link s ty u t) = "{Link "
       ++ show s ++ " "
       ++ show ty ++  " "
       ++ "<" ++ u ++ ">" ++ " "
       ++ show t ++ "} "

  show (Verbatim s verb)    = "{Verb " ++ show s ++ " \"" ++ verb ++ "\"}"
  show (CodeSnippet s code) = "{Code " ++ show s ++ " \"" ++ code ++ "\"}"
  show (MathSnippet s math) = "{Math " ++ show s ++ " \"" ++ math ++ "\"}"

  show (SmallCaps ss) = "{SmallCaps " ++ show ss ++ "}"
  show (SansSerif ss) = "{SansSerif " ++ show ss ++ "}"

  show (Emph ss)   = "{Emph "   ++ show ss ++ "}"
  show (Strong ss) = "{Strong " ++ show ss ++ "}"
  show (Strike ss) = "{Strike " ++ show ss ++ "}"
  show (Uline ss)  = "{Verb "   ++ show ss ++ "}"

  show (Quote qty ss)  = "{" ++ show qty ++ " " ++ show ss ++ "}"
  show (Parens pty ss) = "{" ++ show pty ++ " " ++ show ss ++ "}"

  show (Cite ty id)       = "{" ++ show ty ++ " " ++ id ++ "}"

  show (FNote label desc) = "{FNote \"" ++ label ++ "\" \""
       ++ show desc ++ "\"} "


ListInline : Type
ListInline = List (Inline Prime)

RawListInline : Type
RawListInline = List (Inline Raw)

data Tabular : Step -> Type where
  MkTabular : (s : Step)
            -> Vect n TAlign
            -> Vect m (Vect n (List (Inline s)))
            -> Tabular s

-- incomplete
instance Show (Tabular ty) where
  show tbl = ""

data Block : Step -> Type where
  Para  : (s : Step) -> List (Inline s) -> Block s
  Empty : (s : Step) -> Block s

  Header : (s : Step)
         -> (lvl : Nat)
         -> (label : String)
         -> (title : RawListInline) -> Block s

  Figure : (s : Step)
         -> (label : String)
         -> (caption : List (Inline s))
         -> Maybe Attributes
         -> Inline s
         -> Block s

  TextBlock : (s : Step)
          -> (label : Maybe String)
          -> (caption : Maybe (List (Inline s)))
          -> Maybe Attributes
          -> List (Inline s)
          -> Block s

  VerbBlock : (s : Step)
            -> (label : Maybe String)
            -> (caption : Maybe (List (Inline s)))
            -> Maybe Attributes
            -> String
            -> Block s

  Table : (s : Step)
        -> (label : String)
        -> (caption : List (Inline s))
        -> Tabular s
        -> Block s

  OList : List (Block Prime)  -> Block Prime
  BList : List (Block Prime)  -> Block Prime
  DList : List (ListInline, List (Block Prime)) -> Block Prime

  Listing : (label : Maybe String)
          -> (caption : Maybe (List (Inline Prime)))
          -> Maybe Attributes
          -> String
          -> Block Prime

  Equation : (label : Maybe String) -> String -> Block Prime
  Quotation : ListInline -> Block Prime

  Theorem : (label : Maybe String)
          -> (caption : Maybe ListInline)
          -> TheoremTy
          -> ListInline
          -> Block Prime

instance Show (Block x) where
  show (Para s txt) = "[Para "  ++ show s ++ " " ++ concatMap show txt ++ "]\n"
  show (Empty s)    = "[Empty " ++ show s ++ "]\n"
  show (Header s d l t) = "[Heading " ++ show s ++ " "
       ++ show d ++ " "
       ++ show l ++ " "
       ++ show t ++ "]\n"
  show (Figure s l c as img)  = "[FigBlock "
       ++ show s ++ " "
       ++ show l ++ " "
       ++ show c ++ " "
       ++ show as ++ " "
       ++ show img ++ "]\n"

  show (TextBlock s lab cap as txt) = "[TextBlock "
       ++ show s   ++ " "
       ++ show lab ++ " "
       ++ show cap ++ " "
       ++ show as  ++ " "
       ++ show txt ++ "]\n"

  show (VerbBlock s lab cap as txt) = "[VerbBlock "
       ++ show s   ++ " "
       ++ show lab ++ " "
       ++ show cap ++ " "
       ++ show as  ++ " "
       ++ show txt ++ "]\n"

  show (Listing l cap as co) = "[CodeBlock "
       ++ show l ++ " "
       ++ show cap
       ++ show as ++ " "
       ++ " \"" ++ co ++ "\"]\n"

  show (Table s l c tbl) = "[TblBlock "
       ++ show s ++ " "
       ++ show l ++ " "
       ++ show c ++ " "
       ++ show tbl ++ "]\n"

  show (Theorem l c ty thm) = "[ThmBlock "
       ++ show ty ++ " "
       ++ fromMaybe "" l ++ " "
       ++ show c ++ " "
       ++ concatMap show thm ++ "]\n"

  show (DList ds) = "[DList "
       ++ concatMap (\(k,vs) => show k ++ " "
       ++ concatMap show vs ) ds ++ "]\n"

  show (BList is) = "[BList " ++ concatMap show is ++ "]\n"
  show (OList is) = "[OList " ++ concatMap show is ++ "]\n"

  show (Quotation p)   = "[BQuote " ++ concatMap show p ++ "]\n"
  show (Equation l m)  = "[EqBlock " ++ fromMaybe "" l ++ " \"" ++ m ++ "\"]\n"


data Edda : Step -> Type where
  MkEddaRaw : (ps : Maybe Attributes) -> List (Block Raw) -> Edda Raw
  MkEddaDoc : (ps : Maybe Attributes) -> List (Block Prime) -> Edda Prime

EddaRaw : Type
EddaRaw = Edda Raw

EddaDoc : Type
EddaDoc = Edda Prime

instance Show (Edda x) where
  show (MkEddaDoc ps body) = "[EddaDoc "
       ++ "[Mdata " ++ concatMap show ps ++ "]\n"
       ++ concatMap show body ++ "]\n"

  show (MkEddaRaw ps body) = "[EddaRaw "
       ++ "[MData " ++ concatMap show ps ++ "]\n"
       ++ concatMap show body ++ "]\n"
