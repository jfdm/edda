module Edda.Model.Utils

data QuoteTy = SQuote | DQuote

instance Show QuoteTy where
  show SQuote = "SQuote"
  show DQuote = "DQuote"

data CiteTy = ParenCite | TextCite

instance Show CiteTy where
  show ParenCite = "ParenCite"
  show TextCite  = "TextCite"

data LinkTy = ExLink | InLink

instance Show LinkTy where
  show ExLink = "External"
  show InLink = "Internal"

data ParenTy = Parents | Brackets | Braces

instance Show ParenTy where
  show Parents = "Parens"
  show Brackets = "Brackets"
  show Braces = "Braces"


Attribute : Type
Attribute = (String, String)

Attributes : Type
Attributes = List (String, String)

data PuncTy = Space      | Newline | Tab
            | LBrace     | RBrace
            | RParen     | LParen
            | LBrack     | RBrack
            | LAngle     | RAngle
            | Colon      | Semi
            | EnDash     | EmDash
            | FSlash     | BSlash
            | Plus       | Minus
            | Apostrophe | SMark
            | Pipe
            | Dollar
            | Comma
            | Ellipsis
            | Bang
            | Hyphen
            | Period
            | QMark
            | Hash
            | Equals
            | Other

instance Show PuncTy where
  show Space      = "Space"
  show Newline    = "Newline"
  show Tab        = "Tab"
  show LBrace     = "LBrace"
  show RBrace     = "RBrace"
  show LParen     = "LParen"
  show RParen     = "RParen"
  show LBrack     = "LBrack"
  show RBrack     = "RBrack"
  show LAngle     = "LAngle"
  show RAngle     = "RAngle"
  show Dollar     = "Dollar"
  show Colon      = "Colon"
  show Semi       = "Semi"
  show EnDash     = "EnDash"
  show EmDash     = "EmDash"
  show FSlash     = "Forwardslash"
  show BSlash     = "Backslash"
  show Apostrophe = "Apostrophe"
  show SMark      = "Speech Mark"
  show Comma      = "Comma"
  show Plus       = "Plus"
  show Minus      = "Minus"
  show Ellipsis   = "Ellipsis"
  show Hyphen     = "Hyphen"
  show Bang       = "Bang"
  show Period     = "Period"
  show QMark      = "Question Mark"
  show Hash       = "Hash"
  show Equals     = "Equals"
  show Other      = "Other"
  show Pipe       = "Pipe"


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
