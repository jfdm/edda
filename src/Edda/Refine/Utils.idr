module Edda.Refine.Utils

import Edda.Model
import Edda.Model.Utils


treatLink : String -> List (Inline Prime) -> Inline Prime
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
