module Edda.Effs

import Effects
import Effect.File
import Effect.Exception
import Effect.StdIO


EddaEffs : List EFFECT
EddaEffs = [STDIO, FILE_IO (), EXCEPTION String]
