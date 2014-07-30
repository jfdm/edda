module Edda.Writer.Common

import Effects
import Effect.Exception
import Effect.File
import Effect.StdIO

import Edda.Model

writeEddaFile : (EddaDoc -> {[FILE_IO (OpenFile Write)]} Eff ())
              -> String
              -> EddaDoc -> {[FILE_IO (), EXCEPTION String]} Eff ()
writeEddaFile write fname doc = case !(open fname Write) of
    True => do
      write doc
      close
    False => raise "Unable to create file handle for writing."
