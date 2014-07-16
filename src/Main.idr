module Main

import System

import Effects
import Effect.File
import Effect.StdIO

import Edda.Model
import Edda.Reader
import Edda.Reduce

readShowOrgFile : String -> {[STDIO, FILE_IO ()]} Eff ()
readShowOrgFile fname = do
    case !(readOrgRaw fname) of
      Left err  => putStrLn $ err
      Right res => putStrLn $ show res


main : IO ()
main = do
    args <- getArgs
    case (processArgs args) of
      Just f  => run $ readShowOrgFile f
      Nothing => putStrLn "Wrong"
  where
    processArgs : List String -> Maybe String
    processArgs [x]       = Nothing
    processArgs (x::y::z) = Just y
