module Edda.Refine

import Edda.Model

-- Pairwise reduce
-- Change to effectful state reduce
-- use state to keep track of length of list.
-- x <- reduce list
-- y <- get
-- if length x == y
--   then return
--   else do
--     put $ length x
--     reduce x
reduceBlock : List (Block Raw) -> List (Block Raw)
reduceBlock Nil        = Nil
reduceBlock (x::y::z) = case reduce' x y of
                          Just yes => yes :: reduceBlock z
                          Nothing  => x :: reduceBlock (y::z)
  where
    reduce' : Block Raw -> Block Raw -> Maybe (Block Raw)
    reduce' (RawPara x) (RawPara y) = Just $ RawPara (x ++ y)
    reduce' _           _           = Nothing

{-
reduceList : List (Inline Raw) -> List (Inline Raw)
reduceList Nil  = Nil
reduceList (x::y::z) = case reduce' x y of
                         Just yes => yes :: reduceList z
                         Nothing  =>  x :: reduceList (y::z)
  where
    reduce' : Inline Raw -> Inline Raw -> Maybe (Inline Raw)
    reduce' ()
    reduce' _ _ = Nothing
-}
RefineEffs : List EFFECT
RefineEffs = [STDIO]

refineInline : Inline Raw -> { RefineEffs } Eff (Inline Refined)
refineInline = ()

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
