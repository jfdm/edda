module Edda.Utils

import Edda.Model


lookupAttribute : String -> Attributes -> Maybe Attribute
lookupAttribute key as = find (\(k,v) => k == key) as

lookupValue : String -> Attributes -> Maybe String
lookupValue key as = case lookupAttribute key as of
    Just (k,v) => Just v
    Nothing    => Nothing

lookupType : Attributes -> Maybe String
lookupType = lookupValue "type"

lookupSrcLang : Attributes -> Maybe String
lookupSrcLang = lookupValue "src_lang"
