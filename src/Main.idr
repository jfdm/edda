module Main

import Edda.Model
import Edda.Reader.Org

main : IO ()
main = do
  case parse (many parseInline) "*asas*" of
    Left res  => putStrLn $ res
    Right res => putStrLn $ show res

{-
  case parse eddaOrgReader testDoc of
    Left err => putStrLn err
    Right eDoc => putStrLn $ show eDoc
-}
