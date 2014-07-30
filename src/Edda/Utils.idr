module Edda.Utils

import Edda.Model

getAttr : String -> Maybe Attributes -> Maybe Attribute
getAttr _ Nothing = Nothing
getAttr key (Just as) = find (\(k,v) => k == key) as

getType : Maybe Attributes -> Maybe String
getType as = case getAttr "type" as of
    Just (k,v) => Just v
    Nothing    => Nothing

getSrcLang : Maybe Attributes -> Maybe String
getSrcLang as = case getAttr "src_lang" as of
    Just (k,v) => Just v
    Nothing    => Nothing

getValue : String -> Maybe Attributes -> Maybe String
getValue key as = case getAttr key as of
    Just (k,v) => Just v
    Nothing    => Nothing
