module Main

import System

import TestRunner
import ParsingTest

import OrgReader

-- --------------------------------------------------------------------- [ XML ]
-- -------------------------------------------------------------------- [ Main ]
main : IO ()
main = do
    run $ tests (Z) orgtests
    exit 0

-- --------------------------------------------------------------------- [ EOF ]
