module Edda.Model.Utils

data QuoteTy = SQuote | DQuote

instance Show QuoteTy where
  show SQuote = "SQuote"
  show DQuote = "DQuote"

data CiteSty = ParenSty | TextSty

instance Show CiteSty where
  show ParenSty = "ParenCite"
  show TextSty  = "TextCite"

data ParenTy = Parents | Brackets | Braces

instance Show ParenTy where
  show Parents = "Parens"
  show Brackets = "Brackets"
  show Braces = "Braces"

Attribute : Type
Attribute = (String, String)

Attributes : Type
Attributes = List (String, String)

data TAlign = AlignLeft
            | AlignRight
            | AlignCenter
            | AlignPar

instance Show TAlign where
  show AlignLeft = "l"
  show AlignRight = "r"
  show AlignCenter = "c"
  show AlignPar = "p"

data TheoremTy = Normal
               | Corollary
               | Lemma
               | Proposition
               | Proof
               | Definition
               | Example
               | Exercise
               | Note
               | Problem
               | Question
               | Remark
               | Solution

instance Show TheoremTy where
  show Normal      = "THEOREM"
  show Corollary   = "COROLLARY"
  show Lemma       = "LEMMA"
  show Proposition = "PROPOSITION"
  show Proof       = "PROOF"
  show Definition  = "DEFINITION"
  show Example     = "EXAMPLE"
  show Exercise    = "EXERCISE"
  show Note        = "NOTE"
  show Problem     = "PROBLEM"
  show Question    = "QUESTION"
  show Remark      = "REMARK"
  show Solution    = "SOLUTION"
