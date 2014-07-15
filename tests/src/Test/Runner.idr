module Test.Runner

import Test.Utils

runTests : Show a => List (TestRes a) -> IO ()
runTests [] = do
  putStrLn "All tests completed execution."
  exitWithSuccess
runTests (t::ts) = do
  case t of
    Left err => do
      putStrLn "Test Failure"
      putStrLn $ "Error: " ++ err
      exitWithFailure 1

    Right res => do
      putStrLn $ show res
      runTests ts
