module Test

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Edda.Model
import Edda.Reader.Org

import Data

test : Parser a -> String -> Bool
test p input = case parse p input of
  Left  e => False
  Right x => True

assert : (exp : Bool) -> (p : Parser a) -> (inp : String) -> IO ()
assert exp p inp = case exp == test p inp of
                     True  => putStrLn "Test Pass"
                     False => putStrLn "Test Fail"

namespace Main
  main : IO ()
  main = do
      assert True (many parseInline) testFormatting
      assert True (many parseInline) testInlineLinks
      assert True (many parseInline) testLinks
      assert True (many parseInline) testInline
