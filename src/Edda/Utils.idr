module Edda.Utils

import Edda.Model

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
