module Edda.Reader

import Effects
import Effect.File
import Effect.StdIO

import Edda.Model
import Edda.Reader.Org


-- Maybe Construct a reader class akin to marshal


readFile : { [FILE_IO (OpenFile Read)] } Eff String
readFile = readAcc ""
  where
    readAcc : String -> { [FILE_IO (OpenFile Read)] } Eff String
    readAcc acc = if (not !eof)
                     then readAcc (acc ++ !readLine)
                     else pure acc




readEddaRaw : String -> Parser EddaRaw -> { [FILE_IO ()] } Eff (Either String EddaRaw)
readEddaRaw f p = do
    case !(open f Read) of
      True => do
        src <- readFile
        close
        let res = parse p src
        pure res
      False => pure $ Left "Error"


readShowOrgFile : String -> {[STDIO, FILE_IO ()]} Eff ()
readShowOrgFile fname = do
    case !(readEddaRaw fname parseOrg) of
      Left err  => putStrLn $ err
      Right res => putStrLn $ show res
