module Edda.Model.Internal

data FontTy = SerifTy | SansTy | ScapTy | MonoTy

instance Show FontTy where
  show SerifTy = "Serif"
  show SansTy  = "Sans"
  show ScapTy  = "SmallCaps"
  show MonoTy  = "Monospaced"

data LinkTy = HyperTy | ExposedTy | FnoteTy | RefTy | CiteTy

instance Show LinkTy where
  show HyperTy   = "HyperLink"
  show ExposedTy = "Exposed"
  show FnoteTy   = "Footnote"
  show RefTy     = "Internal"
  show CiteTy    = "Citation"

data MarkupTy = BoldTy | EmphTy | StrikeTy | UlineTy

instance Show MarkupTy where
  show BoldTy   = "Strong"
  show EmphTy   = "Emph"
  show StrikeTy = "Strike"
  show UlineTy  = "Uline"

data RawTy = VerbTy | CodeTy | MathTy

instance Show RawTy where
  show VerbTy = "Verb"
  show CodeTy = "Code"
  show MathTy = "Math"
