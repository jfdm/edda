module Main

import Edda.Model
import Edda.Reader.Org

main : IO ()
main = do
  let doc = """#+TITLE: I am a Title
#+AUTHOR: I am an Author
#+DATE: I is a dat

* Introduction
* Methodology
* Results
* Discussion
* Conclusion
"""
  case parse eddaOrgReader doc of
    Left err => putStrLn err
    Right eDoc => putStrLn $ show eDoc
