module Edda.Utils

import Edda.Model

readTheorem : String -> Maybe TheoremTy
readTheorem thm = case thm of
                    "THEOREM"     => Just Normal
                    "COROLLARY"   => Just Corollary
                    "LEMMA"       => Just Lemma
                    "PROPOSITION" => Just Proposition
                    "PROOF"       => Just Proof
                    "DEFINITION"  => Just Definition
                    "EXAMPLE"     => Just Example
                    "EXERCISE"    => Just Exercise
                    "NOTE"        => Just Note
                    "PROBLEM"     => Just Problem
                    "QUESTION"    => Just Question
                    "REMARK"      => Just Remark
                    "SOLUTION"    => Just Solution
                    otherwise     => Nothing
