module Edda.Refine

import Edda.Model

RefineEffs : List EFFECT
RefineEffs = [STDIO]

refineInline : Inline Raw -> { RefineEffs } Eff (Inline Refined)
refineInline =

refineBlock : RawBlock -> { RefineEffs } Eff BlockRefd
refineBlock = ()

refineDoc : EddaRaw -> { RefineEffs } Eff EddaDoc
refineDoc (MkEdda ps body) = do
    refps   <-
    refBody <- map refineBlock (reduce body)
    pure $ MkEddaDoc refps refBody


refineEdda : EddaRaw -> EddaDoc
refineEdda rdoc = runPure $ refineDoc rdoc


treatLink : String -> List (Inline Refined) -> Inline Refined
treatLink url tar = if length splitURL == 1
                      then Link InLink url (tar)
                      else case head' splitURL of
                        Just "bib"   => (Cite ParenCite url)
                        Just "citet" => (Cite TextCite url)
                        Just "citep" => (Cite ParenCite url)
                        Just _       => (Link ExLink url (tar))
                        Nothing      => (Link ExLink url (tar))
  where
    splitURL : List String
    splitURL = (split (== ':') url)
