module Main

import System

import Effects
import Effect.File
import Effect.StdIO

import Edda.Model
import Edda.Reader

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
