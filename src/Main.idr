module Main

import System

import Effects
import Effect.File
import Effect.StdIO

import Edda.Model
import Edda.Reader
import Edda.Refine
import Edda.Walk

import Debug.Trace

allCaps : Inline s -> Inline s
allCaps (Font ty str) = Font ty $ toUpper str
allCaps x             = x

modHeader : Block s -> Block s
modHeader {s} (Header s n l xs) = if n <= 2
                                    then Para s (map allCaps xs)
                                    else Header s n l xs
modHeader x = x

makeMono : Inline s -> Inline s
makeMono (Font _ str) = Font MonoTy str
makeMono x = x


readShowOrgFile : String -> {[STDIO, FILE_IO ()]} Eff ()
readShowOrgFile fname = do
    d <- readOrgRaw fname
    case d of
      Left err  => putStrLn $ err
      Right res => do
        let nd = walk makeMono res
        putStrLn $ show nd

readShowEddaOrg : String -> {[STDIO, FILE_IO ()]} Eff ()
readShowEddaOrg fname = do
    d <- readOrgRaw fname
    case d of
      Left err  => putStrLn err
      Right doc => do
        putStrLn $ show doc
        case refineEdda doc of
          Left err  => putStrLn err
          Right res => putStrLn $ show res

main : IO ()
main = do
    args <- getArgs
    case (processArgs args) of
      Just f  => run $ readShowEddaOrg f

      Nothing => putStrLn "Wrong"
  where
    processArgs : List String -> Maybe String
    processArgs [x]       = Nothing
    processArgs (x::y::z) = Just y
