module Edda.Effs

import public Effects
import public Effect.File
import public Effect.Exception
import public Effect.StdIO


EddaEffs : List EFFECT
EddaEffs = [STDIO, FILE_IO (), EXCEPTION String]
