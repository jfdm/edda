module Edda.Utils

import Edda.Model

getAttr : String -> Maybe Attributes -> Maybe Attribute
getAttr _ Nothing = Nothing
getAttr key (Just as) = find (\(k,v) => k == key) as

getType : Maybe Attributes -> Maybe String
getType as = case getAttr "type" as of
    Just (k,v) => Just v
    Nothing    => Nothing

readTheorem : String -> Maybe TheoremTy
readTheorem thm = case thm of
                    "THEOREM"     => Just Normal
                    "COROLLARY"   => Just Corollary
                    "LEMMA"       => Just Lemma
                    "PROPOSITION" => Just Proposition
                    "PROOF"       => Just Proof
                    "DEFINITION"  => Just Definition
                    "EXERCISE"    => Just Exercise
                    "NOTE"        => Just Note
                    "PROBLEM"     => Just Problem
                    "QUESTION"    => Just Question
                    "REMARK"      => Just Remark
                    "SOLUTION"    => Just Solution
                    otherwise     => Nothing
