module Test.Data

testTitle : String
testTitle = "#+TITLE: I am a Title\n\n"

testAuthor : String
testAuthor = "#+AUTHOR: I am an Author\n\n"

testDate : String
testDate = "#+DATE: I is a date\n\n"

testHeaders : String
testHeaders = "* Introduction\n\n** Contribution\n\n** Outline\n\n* Methodology\n\n** System\n\n** Procedure\n\n* Results\n\n* Discussion\n\n* Conclusion\n\n"

testFormatting : String
testFormatting = "asasas *bold* /italic/ ~code~ +strike-through+ =verbatim= $mart$ Hello 'Bye bye'.\n"

testLinks : String
testLinks = "[[http://www.cs.st-andrews.ac.uk][URL]] [[http://www.cs.st-andrews.ac.uk][URL]] [[http://www.cs.st-andrews.ac.uk][URL]]\n"

testFNote : String
testFNote = "[fn:label:description]\n"

testInlineLinks : String
testInlineLinks = "[[http://www.cs.st-andrews.ac.uk]] [[Boneh2001]] [[citet:Boneh2001]] [[citep:Boneh2001]]\n"

testInline : String
testInline = testFormatting ++ testLinks ++ testInlineLinks ++ testFNote

testTheo : String
testTheo = "#+CAPTION: sdsd\n#+NAME: asas:asasre\n#+BEGIN_THEOREM\n sdsdsdsd\n#+END_THEOREM"
        ++ "\n#+CAPTION: sdsd\n#+NAME: asas:asasre\n#+BEGIN_COROLLARY\n sdsdsdsd\n#+END_COROLLARY"
        ++ "\n#+CAPTION: sdsd\n#+NAME: asas:asasre\n#+BEGIN_LEMMA\n sdsdsdsd\n#+END_LEMMA"
        ++ "\n#+CAPTION: sdsd\n#+NAME: asas:asasre\n#+BEGIN_PROPOSITION\n sdsdsdsd\n#+END_PROPOSITION"
        ++ "\n#+CAPTION: sdsd\n#+NAME: asas:asasre\n#+BEGIN_PROOF\n sdsdsdsd\n#+END_PROOF"
        ++ "\n#+CAPTION: sdsd\n#+NAME: asas:asasre\n#+BEGIN_DEFINITION\n sdsdsdsd\n#+END_DEFINITION"
        ++ "\n#+CAPTION: sdsd\n#+NAME: asas:asasre\n#+BEGIN_EXAMPLE\n sdsdsdsd\n#+END_EXAMPLE"
        ++ "\n#+CAPTION: sdsd\n#+NAME: asas:asasre\n#+BEGIN_EXERCISE\n sdsdsdsd\n#+END_EXERCISE"
        ++ "\n#+CAPTION: sdsd\n#+NAME: asas:asasre\n#+BEGIN_NOTE\n sdsdsdsd\n#+END_NOTE"
        ++ "\n#+CAPTION: sdsd\n#+NAME: asas:asasre\n#+BEGIN_PROBLEM\n sdsdsdsd\n#+END_PROBLEM"
        ++ "\n#+CAPTION: sdsd\n#+NAME: asas:asasre\n#+BEGIN_QUESTION\n sdsdsdsd\n#+END_QUESTION"
        ++ "\n#+CAPTION: sdsd\n#+NAME: asas:asasre\n#+BEGIN_REMARK\n sdsdsdsd\n#+END_REMARK"
        ++ "\n#+CAPTION: sdsd\n#+NAME: asas:asasre\n#+BEGIN_SOLUTION\n sdsdsdsd\n#+END_SOLUTION\n"

testCode : String
testCode = "#+CAPTION: sdsd\n#+NAME: asas:asasre\n#+BEGIN_SRC idris\n sldkjf;sdjf;dlfj\n#+END_SRC\n\n"
        ++ "#+BEGIN_SRC idris\n sldkjf;sdjf;dlfj\n#+END_SRC\n\n"

testQuote : String
testQuote = "#+BEGIN_QUOTE\n dfkljfzdjf\n#+END_QUOTE\n\n"

testFig : String
testFig = "#+CAPTION: sdsd\n #+NAME: asas:asasre\n[[dfjkdjfldjkf]]\n\n"

testEq : String
testEq = "#+BEGIN_EQUATION\n dfdkfjlkjdfjlk\n#+END_EQUATION\n"
      ++ "#+NAME: asas\n#+BEGIN_EQUATION\n dfdkfjlkjdfjlk\n#+END_EQUATION\n\n"

testBList : String
testBList = """+ a
+ b
+ c
 + a
 + b
 + c
- a
- b
- c
"""

testOList : String
testOList = """1. a
1. b
2. c
 3. a
 3. b
 1. c
2 a
2. b
3. c
"""

testDList : String
testDList = """+ Term :: Description
+ Term :: Description
+ Term :: Description
+ Term :: Description
+ Term :: Description
+ Term :: Description
"""

testBlock : String
testBlock = testHeaders
         ++ testCode
         ++ testQuote
         ++ testFig
         ++ testTheo
         ++ testEq
         ++ testFNote
         ++ testInline
         ++ "End of Test Block\n\n"
