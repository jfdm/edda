module OrgReader

import System

import TestRunner
import ParsingTest

import Edda.Model
import Edda.Reader.Org

-- ------------------------------------------------------------------- [ Tests ]
test : Test
test = do
  parseTestGood (many $ lexeme inline) "asasas *bold* /italic/ ~code~ +strike-through+ =verbatim= $mart$ Hello 'Bye bye'.\n"
  parseTestGood (many inline) "[[http://www.cs.st-andrews.ac.uk][URL]] [[http://www.cs.st-andrews.ac.uk][URL]] [[http://www.cs.st-andrews.ac.uk][URL]]\n"
  parseTestGood (many inline) "[[http://www.cs.st-andrews.ac.uk]] [[Boneh2001]] [[citet:Boneh2001]] [[citep:Boneh2001]]\n"
  parseTestGood (many inline) "[fn:label:description]\n"

test1 : Test
test1 = do
  parseTestGood (attribute "TITLE")  "#+TITLE: I am a Title\n\n"
  parseTestGood (attribute "AUTHOR") "#+AUTHOR: I am an Author\n\n"
  parseTestGood (attribute "DATE")   "#+DATE: I is a date\n\n"

test2 : Test
test2 = parseTestGood (many block) $ "#+CAPTION: sdsd\n#+NAME: asas:asasre\n#+BEGIN_THEOREM\n sdsdsdsd\n#+END_THEOREM"
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

test3 : Test
test3 = do
  parseTestGood (many header) "* Introduction\n\n** Contribution\n\n** Outline\n\n* Methodology\n\n** System\n\n** Procedure\n\n* Results\n\n* Discussion\n\n* Conclusion\n\n"
  parseTestGood (many block) $ "#+CAPTION: sdsd\n#+NAME: asas:asasre\n#+BEGIN_SRC idris\n sldkjf;sdjf;dlfj\n#+END_SRC\n\n"
        ++ "#+BEGIN_SRC idris\n sldkjf;sdjf;dlfj\n#+END_SRC\n\n"
  parseTestGood (block) "#+BEGIN_QUOTE\n dfkljfzdjf\n#+END_QUOTE\n\n"
  parseTestGood (block) "#+CAPTION: sdsd\n #+NAME: asas:asasre\n[[dfjkdjfldjkf]]\n\n"
  parseTestGood (many block)  $ "#+BEGIN_EQUATION\n dfdkfjlkjdfjlk\n#+END_EQUATION\n"
      ++ "#+NAME: asas\n#+BEGIN_EQUATION\n dfdkfjlkjdfjlk\n#+END_EQUATION\n\n"

test4 : Test
test4 = do
  parseTestGood (block) "+ a\n+ b\n+ c\n+ a\n+ b\n+ c\n- a\n- b\n- c\n"
  parseTestGood (block) "1. a\n1. b\n2. c\n3. a\n3. b\n1. c\n2. a\n2. b\n3. c\n"
  parseTestGood (block) "+ Term :: Description+ Term :: Description\n + Term :: Description\n + Term :: Description\n + Term :: Description\n + Term :: Description\n"

orgtests : List Test
orgtests = [test, test1, test2, test3, test4]
