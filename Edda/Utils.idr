module Edda.Utils

import Edda.Model

MkSection : String -> Sentance -> List Block -> Block
MkSection l c bs = Heading 0 l c bs

MkSubSection : String -> Sentance -> List Block -> Block
MkSubSection l c bs = Heading 1 l c bs

MkSubSubSection : String -> Sentance -> List Block -> Block
MkSubSubSection l c bs = Heading 2 l c bs

MkParagraph : String -> Sentance -> List Block -> Block
MkParagraph l c bs = Heading 3 l c bs

MkSubParagraph : String -> Sentance -> List Block -> Block
MkSubParagraph l c bs = Heading 4 l c bs
