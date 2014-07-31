module Edda.Model

data Step = Star | Prime

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
  Font : FontTy   -> String -> Inline Star
  Punc : Char     -> Inline Star
  Link : LinkTy   -> String -> Maybe (List (Inline Star)) -> Inline Star
  Mark : MarkupTy -> List (Inline Star) -> Inline Star
  Raw  : RawTy    -> String -> Inline Star

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
  Hyper : String  -> Maybe (List (Inline Prime)) -> Inline Prime
  FNote : String  -> Maybe (List (Inline Prime)) -> Inline Prime

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

data TextBlockTy = ParaTy | TheoremTy | CorollaryTy | LemmaTy | PropositionTy | ProofTy | DefinitionTy
               | ExerciseTy | NoteTy | ProblemTy | QuestionTy | RemarkTy
               | SolutionTy | ExampleTy | QuotationTy

data VerbBlockTy = CommentTy | ListingTy | LiteralTy | EquationTy

data ListTy = BulletTy | NumberTy

-- @TODO Make blocks multi block
data Block : Step -> Type where
-- Star Constructors
  TextBlock : TextBlockTy
          -> (label : Maybe String)
          -> (caption : Maybe (List (Inline Star)))
          -> Maybe Attributes
          -> List (Inline Star)
          -> Block Star

  VerbBlock : VerbBlockTy
            -> (label : Maybe String)
            -> (caption : Maybe (List (Inline Star)))
            -> Maybe Attributes
            -> String
            -> Block Star

  ListBlock : ListTy
            -> List (List (Inline Star))
            -> Block Star
-- Starry Prime Constructors
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

  Table : (s : Step)
        -> (label : Maybe String)
        -> (caption : Maybe (List (Inline s)))
        -> Tabular
        -> Block s

  DList : (s : Step) -> List (List (Inline s), List (Inline s)) -> Block s
-- Prime Constructors
  OList : List (List (Inline Prime)) -> Block Prime
  BList : List (List (Inline Prime)) -> Block Prime

  Comment : String -> Block Prime
  Equation : (label : Maybe String) -> String -> Block Prime
  Literal : (label : Maybe String)
          -> (caption : Maybe (List (Inline Prime)))
          -> (src : String)
          -> Block Prime
  Listing : (label : Maybe String)
          -> (caption : Maybe (List (Inline Prime)))
          -> (lang : Maybe String)
          -> (langopts : Maybe String)
          -> (as : Maybe Attributes)
          -> (src : String)
         -> Block Prime

  Para        : List (Inline Prime)    -> Block Prime
  Quotation   : (label : Maybe String) -> List (Inline Prime)-> Block Prime

  Theorem     : (label : Maybe String) -> (caption : Maybe (List (Inline Prime))) -> List (Inline Prime) -> Block Prime
  Corollary   : (label : Maybe String) -> (caption : Maybe (List (Inline Prime))) -> List (Inline Prime) -> Block Prime
  Lemma       : (label : Maybe String) -> (caption : Maybe (List (Inline Prime))) -> List (Inline Prime) -> Block Prime
  Proposition : (label : Maybe String) -> (caption : Maybe (List (Inline Prime))) -> List (Inline Prime) -> Block Prime
  Proof       : (label : Maybe String) -> (caption : Maybe (List (Inline Prime))) -> List (Inline Prime) -> Block Prime
  Definition  : (label : Maybe String) -> (caption : Maybe (List (Inline Prime))) -> List (Inline Prime) -> Block Prime
  Exercise    : (label : Maybe String) -> (caption : Maybe (List (Inline Prime))) -> List (Inline Prime) -> Block Prime
  Note        : (label : Maybe String) -> (caption : Maybe (List (Inline Prime))) -> List (Inline Prime) -> Block Prime
  Remark      : (label : Maybe String) -> (caption : Maybe (List (Inline Prime))) -> List (Inline Prime) -> Block Prime
  Problem     : (label : Maybe String) -> (caption : Maybe (List (Inline Prime))) -> List (Inline Prime) -> Block Prime
  Question    : (label : Maybe String) -> (caption : Maybe (List (Inline Prime))) -> List (Inline Prime) -> Block Prime
  Solution    : (label : Maybe String) -> (caption : Maybe (List (Inline Prime))) -> List (Inline Prime) -> Block Prime
  Example     : (label : Maybe String) -> (caption : Maybe (List (Inline Prime))) -> List (Inline Prime) -> Block Prime

data Edda : Step -> Type where
  MkEdda : (s : Step) -> (ps : Maybe Attributes) -> List (Block s) -> Edda s
  MkEddaRaw : (ps : Maybe Attributes) -> List (Block Star) -> Edda Star
  MkEddaDoc : (ps : Maybe Attributes) -> List (Block Prime) -> Edda Prime

EddaDoc : Type
EddaDoc = Edda Prime

EddaRaw : Type
EddaRaw = Edda Star
