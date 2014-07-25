module Edda.Refine

import Edda.Refine.Utils
import Edda.Model
import Edda.Utils
import Edda.Squash

%access private
%default total

-- ---------------------------------------------------------- [ Refine Inlines ]
mutual
  covering
  refineInline : Inline Simple -> (Inline Prime)
  refineInline (Link ty url desc) = case ty of
      RefTy     => Ref url
      ExposedTy => Hyper url [Text url]
      HyperTy   => Hyper url $ fromMaybe [Text "Missing"] $ refineMaybeInlines desc
      FnoteTy   => FNote  url $ fromMaybe [Text "Missing"] $ refineMaybeInlines desc
      CiteTy    => Cite ParenSty url -- <= TODO
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
  refineInlines : List (Inline Simple) -> List (Inline Prime)
  refineInlines is = squash2Inlines $ squash3Inlines $ map refineInline is

  covering
  refineMaybeInlines : Maybe (List (Inline Simple)) -> Maybe (List (Inline Prime))
  refineMaybeInlines Nothing = Nothing
  refineMaybeInlines (Just is) = Just $ refineInlines is

-- ----------------------------------------------------------- [ Refine Blocks ]
-- @TODO Refine Lists
-- @TODO Refine quotes
-- @TODO Refine Parens
mutual
  covering
  refineBlock : Block Simple -> Block Prime
  refineBlock (Empty Simple)             = Empty Prime
  refineBlock (Para  Simple t)           = Para Prime (refineInlines t)
  refineBlock (Header Simple d l t)      = Header Prime d l (refineInlines t)
  refineBlock (Table Simple l c tbl)     = Table Prime l (refineInlines c) tbl
  refineBlock (Figure Simple l c as img) = Figure Prime l (refineInlines c)
                                                          as
                                                          (refineInline img)
  refineBlock (TextBlock Simple l c as t) = case getType as of
    Just "QUOTE" => Quotation l (refineInlines t)
    Just "VERSE" => Quotation l (refineInlines t)
    Just thm     => case readTheorem thm of
                      Just thm' => Theorem l (refineMaybeInlines c)
                                             thm'
                                             (refineInlines t)
                      otherwise => TextBlock Prime l (refineMaybeInlines c)
                                                     as
                                                     (refineInlines t)
    Nothing    => TextBlock Prime l (refineMaybeInlines c) as (refineInlines t)
  refineBlock (VerbBlock Simple l c as s) = case getType as of
    Just "SRC" => Listing l (refineMaybeInlines c) as s
    Just "EQUATION" => Equation l s
    Nothing => VerbBlock Prime l (refineMaybeInlines c) as s
  refineBlock (DList Simple kvs) = DList Prime $ map (\(k, vs) => (refineInlines k, (map refineBlock vs))) kvs
  refineBlock (ListBlock ty bs) = case ty of
    NumberTy => OList $ map refineBlock bs
    BulletTy => BList $ map refineBlock bs

  covering
  refineBlocks : List (Block Simple) -> List (Block Prime)
  refineBlocks bs = squash2Blocks $ map refineBlock bs

-- -------------------------------------------------------------- [ Refine Doc ]
public
covering
refineEdda : EddaRaw -> EddaDoc
refineEdda (MkEdda Simple ps body) = MkEddaDoc ps (refineBlocks body)
refineEdda (MkEddaSimple ps body)  = MkEddaDoc ps (refineBlocks body)
