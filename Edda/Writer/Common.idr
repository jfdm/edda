module Edda.Writer.Common

import Effects
import Effect.Exception
import Effect.File
import Effect.StdIO

import Edda.Model

writeManyThings : (a -> {[FILE_IO (OpenFile Write)]} Eff ())
            -> List a
            -> {[FILE_IO (OpenFile Write)]} Eff ()
writeManyThings _          Nil = pure ()
writeManyThings writeOnce (x::xs) = do
    writeOnce x
    writeManyThings (writeOnce) xs

writeMaybe : (a -> {[FILE_IO (OpenFile Write)]} Eff ())
           -> Maybe a
           -> {[FILE_IO (OpenFile Write)]} Eff ()
writeMaybe _ Nothing  = pure ()
writeMaybe f (Just x) = f x

writeEddaFile : (EddaDoc -> {[FILE_IO (OpenFile Write)]} Eff ())
              -> String
              -> EddaDoc -> {[FILE_IO (), EXCEPTION String]} Eff ()
writeEddaFile write fname doc = case !(open fname Write) of
    True => do
      write doc
      close
    False => raise "Unable to create file handle for writing."
