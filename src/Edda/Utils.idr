module Edda.Utils

import Edda.Model
{-
MkSection : String -> Sentance -> Block
MkSection l c = Heading 0 l c

MkSubSection : String -> Sentance -> Block
MkSubSection l c = Heading 1 l c

MkSubSubSection : String -> Sentance -> Block
MkSubSubSection l c = Heading 2 l c

MkParagraph : String -> Sentance -> Block
MkParagraph l c = Heading 3 l c

MkSubParagraph : String -> Sentance -> Block
MkSubParagraph l c = Heading 4 l c
-}
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
