module Edda.Reader.OrgUtils

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Edda.Model
import Edda.Utils

import Edda.Reader.Utils

treatLink : String -> String -> Inline
treatLink url tar = if length splitURL == 1
                      then Link InLink url (Serif tar)
                      else case head' splitURL of
                        Just "bib"   => (Cite ParenCite url)
                        Just "citet" => (Cite TextCite url)
                        Just "citep" => (Cite ParenCite url)
                        Just _       => (Link ExLink url (Serif tar))
                        Nothing      => (Link ExLink url (Serif tar))
  where
    splitURL : List String
    splitURL = (split (== ':') url)
