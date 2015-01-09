module Edda.Model.Show

import Edda.Model

instance Show Step where
  show SIMPLE = "Simple"
  show PRIME  = "Prime"

instance Show QuoteTy where
  show SQuote = "SQuote"
  show DQuote = "DQuote"

instance Show CiteSty where
  show ParenSty = "ParenCite"
  show TextSty  = "TextCite"

instance Show ParenTy where
  show Parents = "Parens"
  show Brackets = "Brackets"
  show Braces = "Braces"

instance Show FontTy where
  show SerifTy = "Serif"
  show SansTy  = "Sans"
  show ScapTy  = "SmallCaps"
  show MonoTy  = "Monospaced"

instance Show LinkTy where
  show HyperTy   = "HyperLink"
  show ExposedTy = "Exposed"
  show FnoteTy   = "Footnote"
  show RefTy     = "Internal"
  show CiteTy    = "Citation"

instance Show MarkupTy where
  show BoldTy   = "Strong"
  show EmphTy   = "Emph"
  show StrikeTy = "Strike"
  show UlineTy  = "Uline"

instance Show RawTy where
  show VerbTy = "Verb"
  show CodeTy = "Code"
  show MathTy = "Math"

instance Show TextBlockTy where
  show ParaTy        = "PARAGRAPH"
  show TheoremTy     = "THEOREM"
  show CorollaryTy   = "COROLLARY"
  show LemmaTy       = "LEMMA"
  show PropositionTy = "PROPOSITION"
  show ProofTy       = "PROOF"
  show DefinitionTy  = "DEFINITION"
  show ExampleTy     = "EXAMPLE"
  show ExerciseTy    = "EXERCISE"
  show NoteTy        = "NOTE"
  show ProblemTy     = "PROBLEM"
  show QuestionTy    = "QUESTION"
  show RemarkTy      = "REMARK"
  show SolutionTy    = "SOLUTION"
  show QuotationTy   = "QUOTATION"

instance Show VerbBlockTy where
  show CommentTy  = "COMMENT"
  show ListingTy  = "LISTING"
  show LiteralTy  = "LITERTAL"
  show EquationTy = "EQUATION"

instance Show ListTy where
  show BulletTy = "Bullet"
  show NumberTy = "Number"

instance Show (Edda s ty) where
-- ------------------------------------------------------------------ [ Inline ]
  show (Punc c)      = "{Punc " ++ show c  ++ "}"
  show (Font ty t)   = "{Font "   ++ show ty ++ " " ++ show t ++ "}"
  show (Raw ty t)    = "{Raw "    ++ show ty ++ " " ++ show t ++ "}"
  show (Mark ty t)   = "{Mark "   ++ show ty ++ " " ++ show t ++ "}"
  show (Link ty u t) = "{Link "   ++ show ty ++ " <" ++ u ++ "> \"" ++ show t ++ "\"}"

  show (Text text) = "{Text \"" ++ text ++ "\"}"
  show (Mono mono) = "{Mono \"" ++ mono ++ "\"}"
  show (Scap scap) = "{Scap \"" ++ scap ++ "\"}"

  show (Verb verb) = "{Verb \"" ++ verb ++ "\"}"
  show (Code code) = "{Code \"" ++ code ++ "\"}"
  show (Math math) = "{Math \"" ++ math ++ "\"}"

  show (Emph e)   = "{Emph "   ++ show e ++ "}"
  show (Bold b) = "{Strong " ++ show b ++ "}"
  show (Strike s) = "{Strike " ++ show s ++ "}"
  show (Uline u)  = "{Verb "   ++ show u ++ "}"

  show (Quote qty ss)  = "{" ++ show qty ++ " " ++ show ss ++ "}"
  show (Parens pty ss) = "{" ++ show pty ++ " " ++ show ss ++ "}"

  show (Ref l)      = "{Ref " ++ show l ++ "}"
  show (Cite ty id) = "{" ++ show ty ++ " " ++ id ++ "}"
  show (Hyper u d)  = "{Hyper " ++ " <" ++ u ++ "> \"" ++ show d ++ "\"}"
  show (FNote l d)   = "{Fnote " ++ l ++ " \"" ++ show d ++ "\"}"

  show Space      = "{Space}"
  show Newline    = "{Newline}"
  show Tab        = "{Tab}"
  show LBrace     = "{LBrace}"
  show RBrace     = "{RBrace}"
  show LParen     = "{LParen}"
  show RParen     = "{RParen}"
  show LBrack     = "{LBrack}"
  show RBrack     = "{RBrack}"
  show LAngle     = "{LAngle}"
  show RAngle     = "{RAngle}"
  show Dollar     = "{Dollar}"
  show Colon      = "{Colon}"
  show Semi       = "{Semi}"
  show EnDash     = "{EnDash}"
  show EmDash     = "{EmDash}"
  show FSlash     = "{Forwardslash}"
  show BSlash     = "{Backslash}"
  show Apostrophe = "{Apostrophe}"
  show SMark      = "{Speech Mark}"
  show Comma      = "{Comma}"
  show Plus       = "{Plus}"
  show Ellipsis   = "{Ellipsis}"
  show Hyphen     = "{Hyphen}"
  show Bang       = "{Bang}"
  show Period     = "{Period}"
  show QMark      = "{Question Mark}"
  show Hash       = "{Hash}"
  show Equals     = "{Equals}"
  show Pipe       = "{Pipe}"
  show (MiscPunc c) = "{Punc " ++ show c ++"}"
-- ------------------------------------------------------------------ [ BLocks ]
  -- Star
  show (TextBlock ty lab cap as txt) = "[TextBlock "
       ++ show ty  ++ " "
       ++ show lab ++ " "
       ++ show cap ++ " "
       ++ show as  ++ " "
       ++ show txt ++ "]\n"

  show (VerbBlock ty lab cap as txt) = "[VerbBlock "
       ++ show ty  ++ " "
       ++ show lab ++ " "
       ++ show cap ++ " "
       ++ show as  ++ " "
       ++ show txt ++ "]\n"

  show (ListBlock ty is) = "[BList "
       ++ show ty ++ " "
       ++ show is ++ "]\n"

-- Starry Prime
  show (HRule s) = "[HRule" ++ show s ++ "]\n"
  show (Empty s) = "[Empty " ++ show s ++ "]\n"
  show (Section s d l t cs) = "[Heading "
       ++ show s ++ " "
       ++ show d ++ " "
       ++ show l ++ " "
       ++ show t ++ " "
       ++ show cs ++ "]\n"
  show (Figure s l c as img) = "[FigBlock "
       ++ show s ++ " "
       ++ show l ++ " "
       ++ show c ++ " "
       ++ show as ++ " "
       ++ show img ++ "]\n"
  show (DList s ds) = "[DList "
       ++ show s ++ " "
       ++ concatMap (\(k,vs) => show k ++ " " ++ show vs) ds ++ "]\n"
-- Prime
  show (BList is) = "[BList " ++ show is ++ "]\n"
  show (OList is) = "[OList " ++ show is ++ "]\n"

  show (Para txt) = "[Para " ++ show txt ++ "]\n"
  show (Quotation l qs) = "[BQuote "
       ++ show l ++ " "
       ++ show qs ++ "]\n"

  show (Comment cs) = "[Comment "
       ++ show cs ++ "]\n"
  show (Equation l m) = "[EqBlock "
       ++ show l ++ " "
       ++ show m ++ "]\n"
  show (Literal l cap src) = "[Literal "
       ++ show l   ++ " "
       ++ show cap ++ " "
       ++ show src ++ "]\n"
  show (Listing l cap lang ops as co) = "[CodeBlock "
       ++ show l    ++ " "
       ++ show cap  ++ " "
       ++ show lang ++ " "
       ++ show ops  ++ " "
       ++ show as   ++ " "
       ++ show co   ++ "]\n"

  show (Theorem l c txt)     = "[Theorem "     ++ fromMaybe "" l ++ " " ++ show c ++ " " ++ show txt ++ "]\n"
  show (Corollary l c txt)   = "[Corollary "   ++ fromMaybe "" l ++ " " ++ show c ++ " " ++ show txt ++ "]\n"
  show (Lemma l c txt)       = "[Lemma "       ++ fromMaybe "" l ++ " " ++ show c ++ " " ++ show txt ++ "]\n"
  show (Proposition l c txt) = "[Proposition " ++ fromMaybe "" l ++ " " ++ show c ++ " " ++ show txt ++ "]\n"
  show (Proof l c txt)       = "[Proof "       ++ fromMaybe "" l ++ " " ++ show c ++ " " ++ show txt ++ "]\n"
  show (Definition l c txt)  = "[Definition "  ++ fromMaybe "" l ++ " " ++ show c ++ " " ++ show txt ++ "]\n"
  show (Exercise l c txt)    = "[Exercise "    ++ fromMaybe "" l ++ " " ++ show c ++ " " ++ show txt ++ "]\n"
  show (Note l c txt)        = "[Note "        ++ fromMaybe "" l ++ " " ++ show c ++ " " ++ show txt ++ "]\n"
  show (Remark l c txt)      = "[Remark "      ++ fromMaybe "" l ++ " " ++ show c ++ " " ++ show txt ++ "]\n"
  show (Problem l c txt)     = "[Problem "     ++ fromMaybe "" l ++ " " ++ show c ++ " " ++ show txt ++ "]\n"
  show (Solution l c txt)    = "[Question "    ++ fromMaybe "" l ++ " " ++ show c ++ " " ++ show txt ++ "]\n"
  show (Example l c txt)     = "[Example "     ++ fromMaybe "" l ++ " " ++ show c ++ " " ++ show txt ++ "]\n"
