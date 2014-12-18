module Main

import System

import Edda

import Edda.Reader.Org
import Edda.Writer.Org

eddaMain : String -> {EddaEffs} Eff ()
eddaMain fname = with Effects do
    d <- readOrg fname
    case d of
      Left err  => putStrLn $ err
      Right res => do
        putStrLn $ show res
        writeOrg "foobar.org" res

main : IO ()
main = do
    args <- getArgs
    case (processArgs args) of
      Just f  => run $ eddaMain f
      Nothing => putStrLn "Wrong"
  where
    processArgs : List String -> Maybe String
    processArgs [x]       = Nothing
    processArgs (x::y::z) = Just y
