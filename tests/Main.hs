{-# LANGUAGE
OverloadedStrings,
OverloadedLists
  #-}

module Main where


-- Testing
import Test.Tasty
import Test.Tasty.HUnit
-- Text
import Data.Text (Text)
import qualified Data.Text as T
-- Bob-specific
import Bob


main :: IO ()
main = do
  (_, _, mbErrors) <- readData
  defaultMain $ testGroup "tests"
    [ testCase "No warnings when loading rules" $
        null mbErrors @? unlines mbErrors
    , testCase "Simplest rule can be parsed" $ do
        rules <- testReadRules "a = 1: b\n"
        [Rule Nothing [("b", [("a", Top 1)])]] @=? rules
    , testCase "Newline at the end of file isn't needed" $ do
        rules <- testReadRules "a = 1: b"
        [Rule Nothing [("b", [("a", Top 1)])]] @=? rules
    , testCase "A file can be empty" $ do
        rules <- testReadRules ""
        [] @=? rules
    , testCase "A file can contain just a comment" $ do
        rules <- testReadRules "# comment"
        [] @=? rules
    , testCase "Warn when arguments of 'zip' have unequal lengths" $ do
        (_, warnings) <- testReadRulesAndWarnings $ T.unlines [
          "zip abc",
          "    wxyz",
          "    1: ()" ]
        unlines ["warnings in rule at line 1, column 1:",
                 "  lengths of zipped rows don't match" ]
          @=? warnings
    ]

testReadRules :: Text -> IO [Rule]
testReadRules rulesString =
  case readRuleFile Nothing rulesString of
    Left err -> do
      assertFailure (show err)
      error "test failed"
    Right (_rules, Just warning) -> do
      assertFailure warning
      error "test failed"
    Right (rules, Nothing) ->
      return rules

testReadRulesAndWarnings :: Text -> IO ([Rule], Warning)
testReadRulesAndWarnings rulesString =
  case readRuleFile Nothing rulesString of
    Left err -> do
      assertFailure (show err)
      error "test failed"
    Right (rules, Just warning) ->
      return (rules, warning)
    Right (rules, Nothing) ->
      return (rules, "")
