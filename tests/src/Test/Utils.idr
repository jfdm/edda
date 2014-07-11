module Test.Utils

import System

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

TestRes : Type -> Type
TestRes ty = (Either String ty)

parseTest : Show a => Parser a -> String -> TestRes String
parseTest p input = case parse p input of
    Left err => Left err
    Right res => Right $ show res

exitWithFailure : Int -> IO ()
exitWithFailure i = exit i

exitWithSuccess : IO ()
exitWithSuccess = exit 0
