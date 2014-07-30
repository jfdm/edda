module Edda.Reader.Common

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Effects
import Effect.File
import Effect.StdIO

import Edda.Model
import Edda.Refine

%access private

readFile : { [FILE_IO (OpenFile Read)] } Eff String
readFile = readAcc ""
  where
    readAcc : String -> { [FILE_IO (OpenFile Read)] } Eff String
    readAcc acc = if (not !eof)
                     then readAcc (acc ++ !readLine)
                     else pure acc

public
readEddaFile : Parser EddaRaw -> String -> { [FILE_IO ()] } Eff (Either String EddaDoc)
readEddaFile p f = do
    case !(open f Read) of
      True => do
        src <- readFile
        close
        case parse p src of
          Left err  => pure $ Left err
          Right res => pure $ Right (refineEdda res)
      False => pure $ Left "Error"
