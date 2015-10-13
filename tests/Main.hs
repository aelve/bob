module Main where


-- Testing
import Test.Tasty
import Test.Tasty.HUnit
-- Bob-specific
import Bob


main :: IO ()
main = do
  (_, _, mbErrors) <- readData
  defaultMain $ testGroup "tests"
    [ testCase "No warnings when loading rules" $
        null mbErrors @? unlines mbErrors
    ]
