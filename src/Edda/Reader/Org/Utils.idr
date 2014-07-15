module Edda.Reader.Org.Utils

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Edda.Model
import Edda.Utils

import Edda.Reader.Utils

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
