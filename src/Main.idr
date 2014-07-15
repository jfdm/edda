module Main

import System

import Effects
import Effect.File
import Effect.StdIO

import Edda.Model
--import Edda.Reader.Org
import Edda.Reader.Org.Raw

readFile : { [FILE_IO (OpenFile Read)] } Eff String
readFile = readAcc ""
  where
    readAcc : String -> { [FILE_IO (OpenFile Read)] } Eff String
    readAcc acc = if (not !eof)
                     then readAcc (acc ++ !readLine)
                     else pure acc

readOrg : String -> { [FILE_IO ()] } Eff (Either String (EddaRaw))
readOrg f = do
    case !(open f Read) of
      True => do
        src <- readFile
        close
        let res = parse eddaOrgRawReader (src)
        pure res
      False => pure $ Left "Error"

readShowOrgFile : String -> {[STDIO, FILE_IO ()]} Eff ()
readShowOrgFile fname = do
    case !(readOrg fname) of
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
