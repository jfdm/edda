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

lookupSrcOpts : Attributes -> Maybe String
lookupSrcOpts = lookupValue "src_opts"


nubAttribute : String -> Attributes -> Attributes
nubAttribute key as = doNub key as
  where
    doNub : String -> Attributes -> Attributes
    doNub _   Nil     = Nil
    doNub key (x::xs) with (x)
      | (k,v) = case key == k of
                  True => doNub key xs
                  False => x :: doNub key xs
