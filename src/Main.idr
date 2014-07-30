module Main

import System

import Effects
import Effect.File
import Effect.Exception
import Effect.StdIO

import Edda

eddaMain : String -> {[STDIO, FILE_IO (), EXCEPTION String]} Eff ()
eddaMain fname = do
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
