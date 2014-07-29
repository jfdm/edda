module Edda.Model

data Step = Simple | Prime

data FontTy   = SerifTy | SansTy | ScapTy | MonoTy
data QuoteTy  = SQuote | DQuote
data CiteSty  = ParenSty | TextSty
data ParenTy  = Parents | Brackets | Braces
data LinkTy   = HyperTy | ExposedTy | FnoteTy | RefTy | CiteTy
data MarkupTy = BoldTy | EmphTy | StrikeTy | UlineTy
data RawTy    = VerbTy | CodeTy | MathTy

Attribute : Type
Attribute = (String, String)

Attributes : Type
Attributes = List (String, String)

data Inline : Step -> Type where
  Font : FontTy   -> String -> Inline Simple
  Punc : Char     -> Inline Simple
  Link : LinkTy   -> String -> Maybe (List (Inline Simple)) -> Inline Simple
  Mark : MarkupTy -> List (Inline Simple) -> Inline Simple
  Raw  : RawTy    -> String -> Inline Simple

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
  FNote : String  -> List (Inline Prime) -> Inline Prime

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
  EmDash     : Inline Prime
  EnDash     : Inline Prime

  LBrace     : Inline Prime
  RBrace     : Inline Prime
  LParen     : Inline Prime
  RParen     : Inline Prime
  LBrack     : Inline Prime
  RBrack     : Inline Prime

  MiscPunc   : Char -> Inline Prime

data TAlign = AlignLeft | AlignRight | AlignCenter | AlignPar

data Tabular : Type where
  MkTabular : Vect n TAlign
            -> Vect m (Vect n (List String))
            -> Tabular

data TheoremTy = Normal | Corollary | Lemma | Proposition | Proof | Definition
               | Exercise | Note | Problem | Question | Remark
               | Solution

data VerbatimTy = ExampleTy | CommentTy | ListingTy | LiteralTy

data ListTy = BulletTy | NumberTy

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

  ListBlock : ListTy
            -> List (Block Simple)
            -> Block Simple

  DList : (s : Step) -> List (List (Inline s), List (Block s)) -> Block s


  OList : List (Block Prime)  -> Block Prime
  BList : List (Block Prime)  -> Block Prime

  Listing : (label : Maybe String)
          -> (caption : Maybe (List (Inline Prime)))
          -> Maybe Attributes
          -> String
          -> Block Prime

  Equation : (label : Maybe String) -> String -> Block Prime
  Quotation : (label : Maybe String) -> List (Inline Prime)-> Block Prime

  Theorem : (label : Maybe String)
          -> (caption : Maybe (List (Inline Prime)))
          -> TheoremTy
          -> List (Inline Prime)
          -> Block Prime

data Edda : Step -> Type where
  MkEdda : (s : Step) -> (ps : Maybe Attributes) -> List (Block s) -> Edda s
  MkEddaSimple : (ps : Maybe Attributes) -> List (Block Simple) -> Edda Simple
  MkEddaDoc : (ps : Maybe Attributes) -> List (Block Prime) -> Edda Prime

EddaDoc : Type
EddaDoc = Edda Prime

EddaRaw : Type
EddaRaw = Edda Simple
