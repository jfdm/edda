-- --------------------------------------------------------------- [ Utils.idr ]
-- Module    : Utils.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Edda.Utils

import Edda.Model

%access export

lookupType : List (String, String) -> Maybe String
lookupType = lookup "type"

lookupSrcLang : List (String, String) -> Maybe String
lookupSrcLang = lookup "src_lang"

lookupSrcOpts : List (String, String) -> Maybe String
lookupSrcOpts = lookup "src_opts"

nubAttribute : String -> List (String, String) -> List (String, String)
nubAttribute _   Nil = Nil
nubAttribute key as = doNub key as
  where
    doNub : String -> List (String, String) -> List (String, String)
    doNub _   Nil     = Nil
    doNub key (x::xs) with (x)
      | (k,v) = case key == k of
                  True => doNub key xs
                  False => x :: doNub key xs

-- --------------------------------------------------------------------- [ EOF ]
