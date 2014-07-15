module Edda.Model

import Edda.Model.Utils

data Step = Raw | Refined

instance Show Step where
  show Raw     = "Raw"
  show Refined = "Refined"

mutual

  data Inline : Step -> Type where
    RawText : String -> Inline Raw
    RawPunc : PuncTy -> Char -> Inline Raw

    Serif     : String                -> Inline Refined
    SmallCaps : List (Inline Refined) -> Inline Refined
    SansSerif : List (Inline Refined) -> Inline Refined

    Emph   : List (Inline Refined) -> Inline Refined
    Strong : List (Inline Refined) -> Inline Refined
    Strike : List (Inline Refined) -> Inline Refined
    Uline  : List (Inline Refined) -> Inline Refined

    Quote  : QuoteTy -> List (Inline Refined) -> Inline Refined
    Parens : ParenTy -> List (Inline Refined) -> Inline Refined

    Verbatim    : String -> Inline Refined
    CodeSnippet : String -> Inline Refined
    MathSnippet : String -> Inline Refined

    Link  : LinkTy -> String -> List (Inline Refined) -> Inline Refined
    Cite  : CiteTy -> String -> Inline Refined
    FNote : String -> List (Inline Refined) -> Inline Refined

    Punc : PuncTy -> Inline Refined


instance Show (Inline ty) where
  show (Serif s)      = "{Text \""    ++ s       ++ "\"} "
  show (SmallCaps ss) = "{SmallCaps " ++ show ss ++ "} "
  show (SansSerif ss) = "{SansSerif " ++ show ss ++ "} "

  show (Emph ss)   = "{Emph "   ++ show ss ++ "} "
  show (Strong ss) = "{Strong " ++ show ss ++ "} "
  show (Strike ss) = "{Strike " ++ show ss ++ "} "
  show (Uline ss)  = "{Verb "   ++ show ss ++ "} "

  show (Quote qty ss)  = "{" ++ show qty ++ " " ++ show ss ++ "} "
  show (Parens pty ss) = "{" ++ show pty ++ " " ++ show ss ++ "} "

  show (Verbatim verb)    = "{Verb \"" ++ verb ++ "\"} "
  show (CodeSnippet code) = "{Code \"" ++ code ++ "\"} "
  show (MathSnippet math) = "{Math \"" ++ math ++ "\"} "

  show (Cite ty id)       = "{" ++ show ty ++ " " ++ id ++ "} "
  show (Link ty u t)      = "{Link " ++ show ty ++  " <" ++ u ++ "> "
                                                ++ show t ++ "} "
  show (FNote label desc) = "{FNote \""
                         ++ label
                         ++ "\" \""
                         ++ show desc
                         ++ "\"} "

  show (Punc pTy)    = "{Punc \"" ++ show pTy ++ "\"} "

  show (RawText txt)   = "{RawT \"" ++ txt ++ "\"} "
  show (RawPunc pTy c) = "{RawP " ++ case pTy of
                                         Other => "'" ++ show c ++ "'"
                                         _     => show pTy
                                    ++ "} "

ListInline : Type
ListInline = List (Inline Refined)

RawListInline : Type
RawListInline = List (Inline Raw)

data Image : Type where
  MkImage : String -> ListInline -> Image

instance Show Image where
  show (MkImage alt url) = "[Image "
       ++ show alt ++ " "
       ++ show url ++ "]"


mutual
  data Tabular : Type where
    MkTabular : (align : Vect n TAlign)
              -> Vect m (Vect n (List BlockRefd))
              -> Tabular

  data Block :  Step -> Type where
    RawBlock :  (label : Maybe String)
             -> (caption : Maybe RawListInline)
             -> Maybe Attributes
             -> Either String RawListInline -> Block Raw
    RawPara : RawListInline
            -> Block Raw
    RawHeader : (lvl : Nat)
              -> (label : String)
              -> (title : RawListInline) -> Block Raw
    RawFigure : (label : String)
              -> (caption : RawListInline)
              -> Maybe Attributes
              -> RawListInline
              -> Block Raw
    RawEmpty : Block Raw

    Para : ListInline -> Block Refined

    Generic : (label : Maybe String)
            -> (caption : Maybe ListInline)
            -> Maybe Attributes
            -> String
            -> Block Refined

    Listing : (label : Maybe String)
            -> (caption : Maybe ListInline)
            -> Maybe Attributes
            -> String
            -> Block Refined

    Equation : (label : Maybe String) -> String -> Block Refined
    Quotation : ListInline -> Block Refined

    OList : List BlockRefd  -> Block Refined
    BList : List BlockRefd  -> Block Refined
    DList : List (ListInline, List BlockRefd) -> Block Refined

    Figure : (label : String)
           -> (caption : ListInline)
           -> (Maybe Attributes)
           -> Image
           -> Block Refined

    Table : (label : String)
          -> (caption : ListInline)
          -> Tabular
          -> Block Refined

    Theorem : (label : Maybe String)
            -> (caption : Maybe ListInline)
            -> TheoremTy
            -> ListInline
            -> Block Refined

    Heading : (lvl : Nat)
            -> (label : String)
            -> (title : ListInline)
            -> Block Refined

    Empty   : Block Refined

   BlockRaw : Type
   BlockRaw = Block Raw

   BlockRefd : Type
   BlockRefd = Block Refined


instance Show Tabular where
  -- incomplete
  show (MkTabular as cells) = "[Table "
    ++ "[" ++ concatMap (\x => show x ++ " ") as ++ "]"
    ++ " cells ]\n"



{-
    RawBlock :  (label : Maybe String)
             -> (caption : Maybe RawListInline)
             -> Maybe Attributes
             -> Either String RawListInline -> Block Raw
    RawPara : RawListInline -> Block Raw
    RawHeader : (lvl : Nat) -> (title : RawListInline) -> Block Raw

-}

instance Show (Block x)where
  show (RawPara txt) = "[RawPara " ++ concatMap show txt ++ "]\n"
  show (RawBlock lab cap as txt) = "[RawBlock "
       ++ show lab  ++ " "
       ++ show cap ++ " "
       ++ show as   ++ " "
       ++ case txt of
            Left vrb => " \"" ++ vrb ++ "\""
            Right t  => show t
       ++ "]\n"
  show (RawHeader d l t) = "[RawHeading "
       ++ show d ++ " "
       ++ show l ++ " "
       ++ show t ++ "]\n"
  show (RawFigure l c as img)  = "[RawFigBlock "
       ++ show l ++ " "
       ++ show c ++ " "
       ++ show as ++ " "
       ++ show img ++ "]\n"
  show RawEmpty = "[RawEmpty]\n"

  show (Theorem l c ty thm) = "[ThmBlock "
       ++ show ty ++ " "
       ++ fromMaybe "" l ++ " "
       ++ show c ++ " "
       ++ concatMap show thm ++ "]\n"

  show (Table l c tbl) = "[TblBlock "
       ++ show l ++ " "
       ++ show c ++ " "
       ++ show tbl ++ "]\n"

  show (Figure l c as img)  = "[FigBlock "
       ++ show l ++ " "
       ++ show c ++ " "
       ++ show as ++ " "
       ++ show img ++ "]\n"

  show (DList ds) = "[DList "
       ++ concatMap (\(k,vs) => show k ++ " "
       ++ concatMap show vs ) ds ++ "]\n"

  show (BList is) = "[BList " ++ concatMap show is ++ "]\n"
  show (OList is) = "[OList " ++ concatMap show is ++ "]\n"

  show (Quotation p)   = "[BQuote " ++ concatMap show p ++ "]\n"
  show (Equation l m)  = "[EqBlock " ++ fromMaybe "" l ++ " \"" ++ m ++ "\"]\n"

  -- Incomplete
  show (Listing l cap as co) = "[CodeBlock "
       ++ show l ++ " "
       ++ show cap
       ++ " \"" ++ co ++ "\"]\n"

  show (Generic l cap as co) = "[Generic "
       ++ show l ++ " "
       ++ show cap ++ " \"]\n"

  show (Heading d l t) = "[Heading "
       ++ show d ++ " "
       ++ show l ++ " "
       ++ show t ++ "]\n"

  show (Para p) = "[Para " ++ concatMap show p ++ "]\n"
  show Empty = "[Empty]\n"

data Edda : Step -> Type where
  MkEddaRaw : (ps : Maybe Attributes) -> List (Block Raw) -> Edda Raw
  MkEddaDoc : (ps : Maybe Attributes) -> List (Block Refined) -> Edda Refined

EddaRaw : Type
EddaRaw = Edda Raw

EddaDoc : Type
EddaDoc = Edda Refined

instance Show (Edda x) where
  show (MkEddaDoc ps body) = "[EddaDoc "
       ++ "[Mdata " ++ concatMap show ps ++ "]\n"
       ++ concatMap show body ++ "]\n"

  show (MkEddaRaw ps body) = "[EddaRaw "
       ++ "[MData " ++ concatMap show ps ++ "]\n"
       ++ concatMap show body ++ "]\n"
