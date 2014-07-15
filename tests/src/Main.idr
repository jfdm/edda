module Main

import Test.Data
import Test.Utils
import Test.Runner

import Test.OrgReader

main : IO ()
main = do
    runTests orgReaderTests
