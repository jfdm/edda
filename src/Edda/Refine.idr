module Edda.Refine

import Effects
import Effect.STDIO

import Edda.Model
import Edda.Squash

RefineEffs : List EFFECT
RefineEffs = [STDIO]

-- ---------------------------------------------------------- [ Refine Inlines ]

refineInline : Inline Raw -> Inline Prime
refineInline (Verbatim Model.Raw s)    = Verbatim Prime s
refineInline (CodeSnippet Model.Raw s) = CodeSnippet Prime s
refineInline (MathSnippet Model.Raw s) = MathSnippet Prime s
refineInline (Text Model.Raw t) = Text Prime t
--refineInline (Punc Model.Raw pTy c) =


-- ----------------------------------------------------------- [ Refine Blocks ]
refineBlock : Block Raw -> Block Prime
refineBlock (Empty  Model.Raw) = Empty Prime
refineBlock (Para   Model.Raw t) = Para Prime (refineInlines t)
refineBlock (Header Model.Raw d l t) = Header Prime d l (refineInlines t)
refineBlock (Figure Model.Raw l c as img) = Figure Prime l (refineInlines c)
                                                           (refineAttributes as)
                                                           (refineInline img)
refineBlock (TextBlock Model.Raw l c as t) = TextBlock Prime l (refineInlines c)
                                                               (refineAttributes as)
                                                               (refineInlines t)
refineBlock (VerbBlock Model.Raw l c as s) = VerbBlock Prime l (refineInlines c)
                                                               (refineAttributes as)
                                                               s
refineBlock (Table Model.R l c tbl) = Table Prime l (refineInlines c)
                                                    (refineTable tbl)

-- -------------------------------------------------------------- [ Refine Doc ]
refineDoc : EddaRaw -> { RefineEffs } Eff EddaDoc
refineDoc (MkEdda ps body) = do

    refBody <- map refineBlock (reduce body)
    pure $ MkEddaDoc refps refBody

refineEdda : EddaRaw -> EddaDoc
refineEdda rdoc = runPure $ refineDoc rdoc
