module Edda.Model

import Edda.Model.Internal
import Edda.Model.Utils

data Step = Simple | Prime

instance Show Step where
  show Simple  = "Raw"
  show Prime = "Prime"

data Inline : Step -> Type where
  Font : FontTy -> String -> Inline Simple
  Punc : Char -> Inline Simple
  Link : LinkTy -> String -> Maybe (List (Inline Simple)) -> Inline Simple
  Mark : MarkupTy -> List (Inline Simple) -> Inline Simple
  Raw  : RawTy -> String -> Inline Simple

  Text : String -> Inline Prime
  Sans : String -> Inline Prime
  Scap : String -> Inline Prime
  Mono : String -> Inline Prime

  Verb : String -> Inline Prime
  Code : String -> Inline Prime
  Math : String -> Inline Prime

  Emph   : List (Inline Prime) -> Inline Prime
  Bold   : List (Inline Prime) -> Inline Prime
  Strike : List (Inline Prime) -> Inline Prime
  Uline  : List (Inline Prime) -> Inline Prime

  Quote  : QuoteTy -> List (Inline Prime) -> Inline Prime
  Parens : ParenTy -> List (Inline Prime) -> Inline Prime

  Ref   : String  -> Inline Prime
  Cite  : CiteSty -> String -> Inline Prime
  Hyper : String  -> List (Inline Prime) -> Inline Prime
  Note  : String  -> List (Inline Prime) -> Inline Prime

  Space      : Inline Prime
  Newline    : Inline Prime
  Tab        : Inline Prime
  LAngle     : Inline Prime
  RAngle     : Inline Prime
  Colon      : Inline Prime
  Semi       : Inline Prime
  FSlash     : Inline Prime
  BSlash     : Inline Prime
  Apostrophe : Inline Prime
  SMark      : Inline Prime
  Hyphen     : Inline Prime
  EmDash     : Inline Prime
  EnDash     : Inline Prime
  Comma      : Inline Prime
  Plus       : Inline Prime
  Bang       : Inline Prime
  Period     : Inline Prime
  QMark      : Inline Prime
  Hash       : Inline Prime
  Equals     : Inline Prime
  Dollar     : Inline Prime
  Pipe       : Inline Prime
  Ellipsis   : Inline Prime

  LBrace     : Inline Prime
  RBrace     : Inline Prime
  LParen     : Inline Prime
  RParen     : Inline Prime
  LBrack     : Inline Prime
  RBrack     : Inline Prime

  MiscPunc   : Char -> Inline Prime

instance Show (Inline ty) where
  show (Punc c)      = "{Punc \"" ++ show c  ++ "\"}"
  show (Font ty t)   = "{Font "   ++ show ty ++ " " ++ show t ++ "}"
  show (Raw ty t)    = "{Raw "    ++ show ty ++ " " ++ show t ++ "}"
  show (Mark ty t)   = "{Mark "   ++ show ty ++ " " ++ show t ++ "}"
  show (Link ty u t) = "{Link "   ++ show ty ++ " <" ++ u ++ "> \"" ++ show t ++ "\"}"

  show (Text text) = "{Text \"" ++ text ++ "\"}"
  show (Mono mono) = "{Mono \"" ++ mono ++ "\"}"
  show (Scap scap) = "{Scap \"" ++ scap ++ "\"}"

  show (Verb verb) = "{Verb \"" ++ verb ++ "\"}"
  show (Code code) = "{Code \"" ++ code ++ "\"}"
  show (Math math) = "{Math \"" ++ math ++ "\"}"

  show (Emph e)   = "{Emph "   ++ show e ++ "}"
  show (Bold b) = "{Strong " ++ show b ++ "}"
  show (Strike s) = "{Strike " ++ show s ++ "}"
  show (Uline u)  = "{Verb "   ++ show u ++ "}"

  show (Quote qty ss)  = "{" ++ show qty ++ " " ++ show ss ++ "}"
  show (Parens pty ss) = "{" ++ show pty ++ " " ++ show ss ++ "}"

  show (Ref l)      = "{Ref " ++ show l ++ "}"
  show (Cite ty id) = "{" ++ show ty ++ " " ++ id ++ "}"
  show (Hyper u d)  = "{Hyper " ++ " <" ++ u ++ "> \"" ++ show d ++ "\"}"
  show (Note l d)   = "{Fnote " ++ l ++ " \"" ++ show d ++ "\"}"

  show Space      = "{Space}"
  show Newline    = "{Newline}"
  show Tab        = "{Tab}"
  show LBrace     = "{LBrace}"
  show RBrace     = "{RBrace}"
  show LParen     = "{LParen}"
  show RParen     = "{RParen}"
  show LBrack     = "{LBrack}"
  show RBrack     = "{RBrack}"
  show LAngle     = "{LAngle}"
  show RAngle     = "{RAngle}"
  show Dollar     = "{Dollar}"
  show Colon      = "{Colon}"
  show Semi       = "{Semi}"
  show EnDash     = "{EnDash}"
  show EmDash     = "{EmDash}"
  show FSlash     = "{Forwardslash}"
  show BSlash     = "{Backslash}"
  show Apostrophe = "{Apostrophe}"
  show SMark      = "{Speech Mark}"
  show Comma      = "{Comma}"
  show Plus       = "{Plus}"
  show Minus      = "{Minus}"
  show Ellipsis   = "{Ellipsis}"
  show Hyphen     = "{Hyphen}"
  show Bang       = "{Bang}"
  show Period     = "{Period}"
  show QMark      = "{Question Mark}"
  show Hash       = "{Hash}"
  show Equals     = "{Equals}"
  show Pipe       = "{Pipe}"
  show (MiscPunc c) = "{Punc " ++ show c ++"}"

data Tabular : Type where
  MkTabular : Vect n TAlign
            -> Vect m (Vect n (List String))
            -> Tabular

-- incomplete
instance Show Tabular where
  show tbl = ""

data Block : Step -> Type where
  Para  : (s : Step) -> List (Inline s) -> Block s
  Empty : (s : Step) -> Block s

  Header : (s : Step)
         -> (lvl : Nat)
         -> (label : String)
         -> (title : List (Inline s)) -> Block s

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
        -> Tabular
        -> Block s

  OList : List (Block Prime)  -> Block Prime
  BList : List (Block Prime)  -> Block Prime
  DList : List (List (Inline Prime), List (Block Prime)) -> Block Prime

  Listing : (label : Maybe String)
          -> (caption : Maybe (List (Inline Prime)))
          -> Maybe Attributes
          -> String
          -> Block Prime

  Equation : (label : Maybe String) -> String -> Block Prime
  Quotation : List (Inline Prime)-> Block Prime

  Theorem : (label : Maybe String)
          -> (caption : Maybe (List (Inline Prime)))
          -> TheoremTy
          -> List (Inline Prime)
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
  MkEdda : (s : Step) -> (ps : Maybe Attributes) -> List (Block s) -> Edda s
  MkEddaSimple : (ps : Maybe Attributes) -> List (Block Simple) -> Edda Simple
  MkEddaDoc : (ps : Maybe Attributes) -> List (Block Prime) -> Edda Prime

EddaDoc : Type
EddaDoc = Edda Prime

EddaRaw : Type
EddaRaw = Edda Simple

instance Show (Edda x) where
  show (MkEdda s ps body) = "[Edda "
        ++ show s ++ "\n"
       ++ "[Mdata " ++ concatMap show ps ++ "]\n"
       ++ concatMap show body ++ "]\n"

  show (MkEddaDoc ps body) = "[EddaDoc "
       ++ "[Mdata " ++ concatMap show ps ++ "]\n"
       ++ concatMap show body ++ "]\n"

  show (MkEddaSimple ps body) = "[EddaSimple"
       ++ "[MData " ++ concatMap show ps ++ "]\n"
       ++ concatMap show body ++ "]\n"
