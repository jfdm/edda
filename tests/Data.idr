module Test

testDoc : String
testDoc = """#+TITLE: I am a Title
#+AUTHOR: I am an Author
#+DATE: I is a dat
* Introduction
** Contribution
** Outline
* Methodology
** System
** Procedure
* Results
* Discussion
* Conclusion

"""


testHeaders : String
testHeaders = """* Introduction
** Contribution
** Outline
* Methodology
** System
** Procedure
* Results
* Discussion
* Conclusion
"""

testFormatting : String
testFormatting = """
asasas
*bold*
/italic/
=code=
+strike-through+
~verbatim~
$mart$
"Hello"
'Bye bye'
"""

testLinks : String
testLinks = """
[[http://www.cs.st-andrews.ac.uk][URL]]
[[http://www.cs.st-andrews.ac.uk][URL]]
[[http://www.cs.st-andrews.ac.uk][URL]]
"""

testInlineLinks : String
testInlineLinks = """
[[http://www.cs.st-andrews.ac.uk]]
[[Boneh2001]]
[[citet:Boneh2001]]
[[citep:Boneh2001]]
"""
testInline : String
testInline = testFormatting ++ testLinks ++ testInlineLinks

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
