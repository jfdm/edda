module Edda.Refine

import Edda.Model
import Edda.Utils
import Edda.Squash

%access private
%default total

-- ---------------------------------------------------------- [ Refine Inlines ]
-- @TODO Refine quotes
-- @TODO Refine Parens

treatPunc : Char -> Maybe (Edda PRIME INLINE)
treatPunc c = case c of
    ' '   => Just Space
    '\n'  => Just Newline
    '\t'  => Just Tab
    '<'   => Just LAngle
    '>'   => Just RAngle
    ':'   => Just Colon
    ';'   => Just Semi
    '/'   => Just FSlash
    '\\'  => Just BSlash
    '\''  => Just Apostrophe
    '-'   => Just Hyphen
    ','   => Just Comma
    '+'   => Just Plus
    '!'   => Just Bang
    '.'   => Just Period
    '?'   => Just QMark
    '#'   => Just Hash
    '='   => Just Equals
    '$'   => Just Dollar
    '|'   => Just Pipe

    '{' => Just LBrace
    '}' => Just RBrace
    '(' => Just LParen
    ')' => Just RParen
    '[' => Just LBrack
    ']' => Just RBrack
    '"' => Just SMark
    otherwise => Nothing

mutual
  covering
  refineInline : Edda STAR INLINE -> Edda PRIME INLINE
  refineInline (Link ty url desc) = case ty of
      RefTy     => Ref url
      ExposedTy => Hyper url Nil
      HyperTy   => Hyper url $ refineInlines desc
      FnoteTy   => FNote url $ refineInlines desc
      CiteTy    => Cite ParenSty url -- <= @TODO
  refineInline (Mark ty txt) = case ty of
      BoldTy   => Bold   $ refineInlines txt
      EmphTy   => Emph   $ refineInlines txt
      StrikeTy => Strike $ refineInlines txt
      UlineTy  => Uline  $ refineInlines txt
  refineInline (Punc c) = case treatPunc c of
      Just p  => p
      Nothing => MiscPunc c
  refineInline (Font ty s) = case ty of
      SerifTy => Text s
      SansTy  => Sans s
      ScapTy  => Scap s
      MonoTy  => Mono s
  refineInline (Raw ty v) = case ty of
      VerbTy => Verb v
      CodeTy => Code v
      MathTy => Math v

  covering
  public
  refineInlines : List (Edda STAR INLINE) -> List (Edda PRIME INLINE)
  refineInlines Nil = Nil
  refineInlines is = squash2 $ squash3 $ map refineInline is

-- ----------------------------------------------------------- [ Refine Blocks ]
mutual
  covering
  refineBlock : Edda STAR BLOCK -> Edda PRIME BLOCK
  refineBlock (HRule STAR)            = HRule PRIME
  refineBlock (Empty STAR)            = Empty PRIME
  refineBlock (Section STAR d l t as) = Section PRIME d l (refineInlines t) as
  refineBlock (Figure STAR l c as img) = Figure PRIME l (refineInlines c)
                                                         as
                                                         (refineInline img)
  refineBlock (DList STAR kvs) = DList PRIME $ map (\(k, vs) => (refineInlines k, refineInlines vs)) kvs

  refineBlock (TextBlock ty l c as t) = case ty of
    ParaTy        => Para (refineInlines t)
    QuotationTy   => Quotation   l (refineInlines t)
    TheoremTy     => Theorem     l (refineInlines c) (refineInlines t)
    CorollaryTy   => Corollary   l (refineInlines c) (refineInlines t)
    LemmaTy       => Lemma       l (refineInlines c) (refineInlines t)
    PropositionTy => Proposition l (refineInlines c) (refineInlines t)
    ProofTy       => Proof       l (refineInlines c) (refineInlines t)
    DefinitionTy  => Definition  l (refineInlines c) (refineInlines t)
    ExerciseTy    => Exercise    l (refineInlines c) (refineInlines t)
    NoteTy        => Note        l (refineInlines c) (refineInlines t)
    ProblemTy     => Problem     l (refineInlines c) (refineInlines t)
    QuestionTy    => Question    l (refineInlines c) (refineInlines t)
    RemarkTy      => Remark      l (refineInlines c) (refineInlines t)
    SolutionTy    => Solution    l (refineInlines c) (refineInlines t)
    ExampleTy     => Example     l (refineInlines c) (refineInlines t)

  refineBlock (VerbBlock ty l c as s) = case ty of
    CommentTy  => Comment s
    ListingTy  => Listing l (refineInlines c)
                            (lookupSrcLang as)
                            (lookupSrcOpts as)
                            ((nubAttribute "src_lang" $ nubAttribute "src_opts" as))
                            s
    LiteralTy  => Literal l (refineInlines c) s
    EquationTy => Equation l s

  refineBlock (ListBlock ty bs) = case ty of
    NumberTy => OList $ map refineInlines bs
    BulletTy => BList $ map refineInlines bs

  covering
  public
  refineBlocks : List (Edda STAR BLOCK) -> List (Edda PRIME BLOCK)
  refineBlocks Nil = Nil
  refineBlocks bs = squash2 $ map refineBlock bs

-- -------------------------------------------------------------- [ Refine Doc ]
public
covering
refineEdda : Edda STAR MODEL -> Edda PRIME MODEL
refineEdda (EddaRaw ps body) = MkEdda ps (refineBlocks body)
