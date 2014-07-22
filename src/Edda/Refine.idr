module Edda.Refine

import Effects
import Effect.STDIO

import Edda.Model
import Edda.Squash

RefineEffs : List EFFECT
RefineEffs = [STDIO]

-- ---------------------------------------------------------- [ Refine Inlines ]

mutual
  treatLink : LinkTy -> String -> Maybe (List (Inline Simple)) -> Inline Prime
  treatLink ty url desc = case ty of
      RefTy     => Ref   url
      ExposedTy => Hyper url (Text url)
      HyperTy   => Hyper url $ map refineInline (fromMaybe [Text "Missing"] desc)
      FnoteTy   => Note  url $ map refineInline (fromMaybe [Text "Missing"] desc)
      CiteTy    => Cite ParenSty url -- <= TODO


  refineInline : Inline Simple -> Inline Prime
  refineInline (Font ty s) = case ty of
      SerifTy => Text s
      SansTy  => Sans s
      ScapTy  => Scap s
      MonoTy  => Mono s
  refineInline (Punc c) = case treatPunc c of
      Just p  => p
      Nothing => MiscPunc c
  refineInline (Link ty url desc) = treatLink ty url desc
  refineInline (Mark ty txt) = case ty of
      BoldTy   => Bold   $ map refineInline txt
      EmphTy   => Emph   $ map refineInline txt
      StrikeTy => Strike $ map refineInline txt
      UlineTy  => Uline  $ map refineInline txt
  refineInline (Raw ty v) = case ty of
      VerbTy => Verb v
      CodeTy => Code v
      MathTy => Math v

refineInlines : List (Inline Simple) -> List (Inline Prime)
refineInlines = map refineInline

-- ----------------------------------------------------------- [ Refine Blocks ]
refineBlock : Block Simple -> Block Prime
refineBlock (Empty Simple)         = Empty Prime
refineBlock (Para  Simple t)       = Para Prime (refineInlines t)
refineBlock (Header Simple d l t)  = Header Prime d l (refineInlines t)
refineBlock (Table Simple l c tbl) = Table Prime l (refineInlines c) tbl
refineBlock (Figure Simple l c as img) = Figure Prime l (refineInlines c)
                                                        (refineAttributes as)
                                                        (refineInline img)
refineBlock (TextBlock Simple l c as t) = case getAttr

TextBlock Prime l (refineInlines c)
                                                               (refineAttributes as)
                                                               (refineInlines t)
refineBlock (VerbBlock Simple l c as s) = VerbBlock Prime l (refineInlines c)
                                                               (refineAttributes as)
                                                               s



-- -------------------------------------------------------------- [ Refine Doc ]
refineDoc : EddaRaw -> { RefineEffs } Eff EddaDoc
refineDoc (MkEdda ps body) = do

    refBody <- map refineBlock (reduce body)
    pure $ MkEddaDoc refps refBody

refineEdda : EddaRaw -> EddaDoc
refineEdda rdoc = runPure $ refineDoc rdoc
