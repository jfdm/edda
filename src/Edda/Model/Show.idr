module Edda.Model.Show

import Edda.Model

instance Show Step where
  show Simple = "Simple"
  show Prime  = "Prime"

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

instance Show (Inline ty) where
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

instance Show TAlign where
  show AlignLeft = "l"
  show AlignRight = "r"
  show AlignCenter = "c"
  show AlignPar = "p"

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

instance Show ListTy where
  show BulletTy = "Bullet"
  show NumberTy = "Number"

-- incomplete
instance Show Tabular where
  show tbl = ""

instance Show (Block x) where
  show (Para s txt) = "[Para "  ++ show s ++ " " ++ concatMap show txt ++ "]\n"
  show (Empty s)    = "[Empty " ++ show s ++ "]\n"
  show (Header s d l t) = "[Heading " ++ show s ++ " "
       ++ show d ++ " "
       ++ show l ++ " "
       ++ show t ++ "]\n"
  show (Figure s l c as img)  = "[FigBlock "
       ++ show s ++ " "
       ++ show l ++ " "
       ++ show c ++ " "
       ++ show as ++ " "
       ++ show img ++ "]\n"

  show (TextBlock s lab cap as txt) = "[TextBlock "
       ++ show s   ++ " "
       ++ show lab ++ " "
       ++ show cap ++ " "
       ++ show as  ++ " "
       ++ show txt ++ "]\n"

  show (VerbBlock s lab cap as txt) = "[VerbBlock "
       ++ show s   ++ " "
       ++ show lab ++ " "
       ++ show cap ++ " "
       ++ show as  ++ " "
       ++ show txt ++ "]\n"

  show (Listing l cap as co) = "[CodeBlock "
       ++ show l ++ " "
       ++ show cap
       ++ show as ++ " "
       ++ " \"" ++ co ++ "\"]\n"

  show (Table s l c tbl) = "[TblBlock "
       ++ show s ++ " "
       ++ show l ++ " "
       ++ show c ++ " "
       ++ show tbl ++ "]\n"

  show (Theorem l c ty thm) = "[ThmBlock "
       ++ show ty ++ " "
       ++ fromMaybe "" l ++ " "
       ++ show c ++ " "
       ++ concatMap show thm ++ "]\n"

  show (DList s ds) = "[DList "
       ++ show s ++ " "
       ++ concatMap (\(k,vs) => show k ++ " "
       ++ concatMap show vs ) ds ++ "]\n"

  show (ListBlock ty is) = "[BList " ++ show ty ++ " " ++ concatMap show is ++ "]\n"

  show (BList is) = "[BList " ++ concatMap show is ++ "]\n"
  show (OList is) = "[OList " ++ concatMap show is ++ "]\n"

  show (Quotation l p) = "[BQuote " ++ fromMaybe "" l ++ " " ++concatMap show p ++ "]\n"
  show (Equation l m)  = "[EqBlock " ++ fromMaybe "" l ++ " \"" ++ m ++ "\"]\n"

instance Show (Edda ty) where
  show (MkEdda step ps body) = "[Edda "
       ++ show step ++ "\n"
       ++ "[Mdata " ++ concatMap show ps ++ "]\n"
       ++ concatMap show body ++ "]\n"

  show (MkEddaDoc ps body) = "[EddaDoc "
       ++ "[Mdata " ++ concatMap show ps ++ "]\n"
       ++ concatMap show body ++ "]\n"

  show (MkEddaSimple ps body) = "[EddaSimple"
       ++ "[MData " ++ concatMap show ps ++ "]\n"
       ++ concatMap show body ++ "]\n"
