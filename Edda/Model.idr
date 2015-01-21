module Edda.Model

data Step = STAR | PRIME

data FontTy   = SerifTy | SansTy | ScapTy | MonoTy
data QuoteTy  = SQuote | DQuote
data CiteSty  = ParenSty | TextSty
data ParenTy  = Parents | Brackets | Braces
data LinkTy   = HyperTy | ExposedTy | FnoteTy | RefTy | CiteTy
data MarkupTy = BoldTy | EmphTy | StrikeTy | UlineTy
data RawTy    = VerbTy | CodeTy | MathTy

data TextBlockTy = ParaTy | TheoremTy | CorollaryTy | LemmaTy | PropositionTy | ProofTy | DefinitionTy
               | ExerciseTy | NoteTy | ProblemTy | QuestionTy | RemarkTy
               | SolutionTy | ExampleTy | QuotationTy

data VerbBlockTy = CommentTy | ListingTy | LiteralTy | EquationTy

data ListTy = BulletTy | NumberTy

||| Add different block types but that will require adding predicated lists
data EddaTy = INLINE | BLOCK | MODEL

||| Support Tables

data Edda : Step -> EddaTy -> Type where
-- ------------------------------------------------------------------ [ Inline ]
-- --------------------------------------------------------------------- [ Raw ]
  Font : FontTy   -> String -> Edda STAR INLINE
  Punc : Char     -> Edda STAR INLINE
  Link : LinkTy   -> String -> List (Edda STAR INLINE) -> Edda STAR INLINE
  Mark : MarkupTy -> List (Edda STAR INLINE) -> Edda STAR INLINE
  Raw  : RawTy    -> String -> Edda STAR INLINE
-- --------------------------------------------------------------- [ Processed ]
  Text : String -> Edda PRIME INLINE
  Sans : String -> Edda PRIME INLINE
  Scap : String -> Edda PRIME INLINE
  Mono : String -> Edda PRIME INLINE

  Verb : String -> Edda PRIME INLINE
  Code : String -> Edda PRIME INLINE
  Math : String -> Edda PRIME INLINE

  Emph   : List (Edda PRIME INLINE) -> Edda PRIME INLINE
  Bold   : List (Edda PRIME INLINE) -> Edda PRIME INLINE
  Strike : List (Edda PRIME INLINE) -> Edda PRIME INLINE
  Uline  : List (Edda PRIME INLINE) -> Edda PRIME INLINE

  Quote  : QuoteTy -> List (Edda PRIME INLINE) -> Edda PRIME INLINE
  Parens : ParenTy -> List (Edda PRIME INLINE) -> Edda PRIME INLINE

  Ref   : String  -> Edda PRIME INLINE
  Cite  : CiteSty -> String -> Edda PRIME INLINE
  Hyper : String  -> List (Edda PRIME INLINE) -> Edda PRIME INLINE
  FNote : String  -> List (Edda PRIME INLINE) -> Edda PRIME INLINE

  Space      : Edda PRIME INLINE
  Newline    : Edda PRIME INLINE
  Tab        : Edda PRIME INLINE
  LAngle     : Edda PRIME INLINE
  RAngle     : Edda PRIME INLINE
  Colon      : Edda PRIME INLINE
  Semi       : Edda PRIME INLINE
  FSlash     : Edda PRIME INLINE
  BSlash     : Edda PRIME INLINE
  Apostrophe : Edda PRIME INLINE
  SMark      : Edda PRIME INLINE
  Hyphen     : Edda PRIME INLINE
  Comma      : Edda PRIME INLINE
  Plus       : Edda PRIME INLINE
  Bang       : Edda PRIME INLINE
  Period     : Edda PRIME INLINE
  QMark      : Edda PRIME INLINE
  Hash       : Edda PRIME INLINE
  Equals     : Edda PRIME INLINE
  Dollar     : Edda PRIME INLINE
  Pipe       : Edda PRIME INLINE
  Ellipsis   : Edda PRIME INLINE
  EmDash     : Edda PRIME INLINE
  EnDash     : Edda PRIME INLINE

  LBrace     : Edda PRIME INLINE
  RBrace     : Edda PRIME INLINE
  LParen     : Edda PRIME INLINE
  RParen     : Edda PRIME INLINE
  LBrack     : Edda PRIME INLINE
  RBrack     : Edda PRIME INLINE

  MiscPunc   : Char -> Edda PRIME INLINE
-- ------------------------------------------------------------------ [ Blocks ]
-- --------------------------------------------------------------------- [ Raw ]
  TextBlock : TextBlockTy
            -> Maybe String
            -> List (Edda STAR INLINE)
            -> List (String, String)
            -> List (Edda STAR INLINE)
            -> Edda STAR BLOCK
  VerbBlock : VerbBlockTy
            -> Maybe String
            -> List (Edda STAR INLINE)
            -> List (String, String)
            -> String
            -> Edda STAR BLOCK
  ListBlock : ListTy -> List (List (Edda STAR INLINE)) -> Edda STAR BLOCK
-- ------------------------------------------------------------------- [ Mixed ]
  HRule : (s : Step) -> Edda s BLOCK
  Empty : (s : Step) -> Edda s BLOCK

  Figure : (s : Step) -> String -> List (Edda s INLINE) -> List (String, String) -> Edda s INLINE -> Edda s BLOCK
  DList  : (s : Step) -> List (List (Edda s INLINE), List (Edda s INLINE)) -> Edda s BLOCK

  Section : (s : Step)
          -> Nat
          -> Maybe String
          -> List (Edda s INLINE)
          -> List (String, String)
          -> List (Edda s BLOCK) -> Edda s BLOCK

-- --------------------------------------------------------------- [ Processed ]

  OList : List (List (Edda PRIME INLINE)) -> Edda PRIME BLOCK
  BList : List (List (Edda PRIME INLINE)) -> Edda PRIME BLOCK

  Comment : String -> Edda PRIME BLOCK
  Equation : Maybe String -> String -> Edda PRIME BLOCK
  Literal  : Maybe String -> List (Edda PRIME INLINE) -> String -> Edda PRIME BLOCK
  Listing : Maybe String
          -> (List (Edda PRIME INLINE))
          -> (Maybe String)
          -> (Maybe String)
          -> List (String, String)
          -> String
          -> Edda PRIME BLOCK

  Para : List (Edda PRIME INLINE) -> Edda PRIME BLOCK

  Quotation   : Maybe String -> List (Edda PRIME INLINE)-> Edda PRIME BLOCK

  Theorem     : Maybe String -> List (Edda PRIME INLINE) -> List (Edda PRIME INLINE) -> Edda PRIME BLOCK
  Corollary   : Maybe String -> List (Edda PRIME INLINE) -> List (Edda PRIME INLINE) -> Edda PRIME BLOCK
  Lemma       : Maybe String -> List (Edda PRIME INLINE) -> List (Edda PRIME INLINE) -> Edda PRIME BLOCK
  Proposition : Maybe String -> List (Edda PRIME INLINE) -> List (Edda PRIME INLINE) -> Edda PRIME BLOCK
  Proof       : Maybe String -> List (Edda PRIME INLINE) -> List (Edda PRIME INLINE) -> Edda PRIME BLOCK
  Definition  : Maybe String -> List (Edda PRIME INLINE) -> List (Edda PRIME INLINE) -> Edda PRIME BLOCK
  Exercise    : Maybe String -> List (Edda PRIME INLINE) -> List (Edda PRIME INLINE) -> Edda PRIME BLOCK
  Note        : Maybe String -> List (Edda PRIME INLINE) -> List (Edda PRIME INLINE) -> Edda PRIME BLOCK
  Remark      : Maybe String -> List (Edda PRIME INLINE) -> List (Edda PRIME INLINE) -> Edda PRIME BLOCK
  Problem     : Maybe String -> List (Edda PRIME INLINE) -> List (Edda PRIME INLINE) -> Edda PRIME BLOCK
  Question    : Maybe String -> List (Edda PRIME INLINE) -> List (Edda PRIME INLINE) -> Edda PRIME BLOCK
  Solution    : Maybe String -> List (Edda PRIME INLINE) -> List (Edda PRIME INLINE) -> Edda PRIME BLOCK
  Example     : Maybe String -> List (Edda PRIME INLINE) -> List (Edda PRIME INLINE) -> Edda PRIME BLOCK


-- --------------------------------------------------------------- [ Documents ]
  EddaRaw : List (String, String) -> List (Edda STAR BLOCK) -> Edda STAR MODEL
  MkEdda  : List (String, String) -> List (Edda PRIME BLOCK) -> Edda PRIME MODEL
