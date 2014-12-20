module Edda.Refine

import Edda.Refine.Utils
import Edda.Model
import Edda.Utils
import Edda.Squash

%access private
%default total

-- ---------------------------------------------------------- [ Refine Inlines ]
-- @TODO Refine quotes
-- @TODO Refine Parens

mutual
  covering
  refineInline : Inline Star -> (Inline Prime)
  refineInline (Link ty url desc) = case ty of
      RefTy     => Ref url
      ExposedTy => Hyper url Nothing
      HyperTy   => Hyper url $ refineMaybeInlines desc
      FnoteTy   => FNote url $ refineMaybeInlines desc
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
  refineInlines : List (Inline Star) -> List (Inline Prime)
  refineInlines is = squash2Inlines $ squash3Inlines $ map refineInline is

  covering
  refineMaybeInlines : Maybe (List (Inline Star)) -> Maybe (List (Inline Prime))
  refineMaybeInlines Nothing = Nothing
  refineMaybeInlines (Just is) = Just $ refineInlines is

-- ----------------------------------------------------------- [ Refine Blocks ]
mutual
  covering
  refineBlock : Block Star -> Block Prime
  refineBlock (HRule Star)             = HRule Prime
  refineBlock (Empty Star)             = Empty Prime
  refineBlock (Header Star d l t)      = Header Prime d l (refineInlines t)
  refineBlock (Table Star l c tbl)     = Table Prime l (refineMaybeInlines c) tbl
  refineBlock (Figure Star l c as img) = Figure Prime l (refineInlines c)
                                                          as
                                                          (refineInline img)
  refineBlock (DList Star kvs) = DList Prime $ map (\(k, vs) => (refineInlines k, refineInlines vs)) kvs

  refineBlock (TextBlock ty l c as t) = case ty of
    ParaTy        => Para (refineInlines t)
    QuotationTy   => Quotation l (refineInlines t)
    TheoremTy     => Theorem l (refineMaybeInlines c) (refineInlines t)
    CorollaryTy   => Corollary l (refineMaybeInlines c) (refineInlines t)
    LemmaTy       => Lemma l (refineMaybeInlines c) (refineInlines t)
    PropositionTy => Proposition l (refineMaybeInlines c) (refineInlines t)
    ProofTy       => Proof l (refineMaybeInlines c) (refineInlines t)
    DefinitionTy  => Definition l (refineMaybeInlines c) (refineInlines t)
    ExerciseTy    => Exercise l (refineMaybeInlines c) (refineInlines t)
    NoteTy        => Note l (refineMaybeInlines c) (refineInlines t)
    ProblemTy     => Problem l (refineMaybeInlines c) (refineInlines t)
    QuestionTy    => Question l (refineMaybeInlines c) (refineInlines t)
    RemarkTy      => Remark l (refineMaybeInlines c) (refineInlines t)
    SolutionTy    => Solution l (refineMaybeInlines c) (refineInlines t)
    ExampleTy     => Example l (refineMaybeInlines c) (refineInlines t)

  refineBlock (VerbBlock ty l c as s) = case ty of
    CommentTy  => Comment s
    ListingTy  => case as of
      Nothing  => Listing l (refineMaybeInlines c) Nothing Nothing Nothing s
      Just ops => Listing l (refineMaybeInlines c)
                            (lookupSrcLang ops)
                            (lookupSrcOpts ops)
                            (Just (nubAttribute "src_lang" $ nubAttribute "src_opts" ops)) s
    LiteralTy  => Literal l (refineMaybeInlines c) s
    EquationTy => Equation l s

  refineBlock (ListBlock ty bs) = case ty of
    NumberTy => OList $ map refineInlines bs
    BulletTy => BList $ map refineInlines bs

  covering
  refineBlocks : List (Block Star) -> List (Block Prime)
  refineBlocks bs = squash2Blocks $ map refineBlock bs

-- -------------------------------------------------------------- [ Refine Doc ]
public
covering
refineEdda : EddaRaw -> EddaDoc
refineEdda (MkEdda Star ps body) = MkEddaDoc ps (refineBlocks body)
