{-# LANGUAGE
OverloadedStrings,
OverloadedLists
  #-}

module Main where


-- Testing
import Test.Tasty
import Test.Tasty.HUnit
-- Lists
import Data.List (intercalate, dropWhileEnd)
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
    , parsingTests
    , behaviorTests
    , warningTests
    ]

parsingTests :: TestTree
parsingTests = testGroup "parsing"
  [ testCase "Simplest rule can be parsed" $ do
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
  ]

behaviorTests :: TestTree
behaviorTests = testGroup "behavior"
  [ testCase "####-notes should work" $ do
      rules <- testReadRules $ T.unlines [
        "####  blah",
        "a = 1: b" ]
      [Rule (Just "blah") [("b", [("a", Top 1)])]] @=? rules
  ]

warningTests :: TestTree
warningTests = testGroup "warnings"
  [ testCase "Warn when arguments of 'zip' have unequal lengths" $ do
      (_, warnings) <- testReadRulesAndWarnings $ T.unlines [
        "zip abc",
        "    wxyz",
        "    1: ()" ]
      unlines ["warnings in rule at line 1, column 1:",
               "  lengths of zipped rows don't match" ]
        @=? warnings
  , testCase "Warn when undefined things are referenced" $ do
      (_, warnings) <- testReadRulesAndWarnings $ T.unlines [
        "a   : 1 2 3",
        "`e` : x y z" ]
      unlines ["warnings in rule at line 1, column 1:",
               "  ‘e’ was referenced but wasn't defined yet" ]
        @=? warnings
  , testCase "Warn when no value can be provided for a variable" $ do
      (_, warnings) <- testReadRulesAndWarnings $ T.unlines [
        "a  : 1 2 3",
        "() : x y z" ]
      unlines ["warnings in rule at line 1, column 1:",
               "  there's a variable in the rule but no value provided for it" ]
        @=? warnings
  , testCase "Warn when empty patterns are encountered" $ do
      (_, warnings) <- testReadRulesAndWarnings $ T.unlines [
        "a  : 1 2 3",
        "'' : x y z" ]
      unlines ["warnings in rule at line 1, column 1:",
               "  matcher #2 contains an empty pattern" ]
        @=? warnings
  , testCase "Warn when priorities aren't satisfied" $ do
      (_, warnings) <- testReadRulesAndWarnings $ T.unlines [
        "e1 = 1: x",
        "     2: y",
        "e2 = 1: x",
        "     2: y",
        "e3 = 2: x",
        "     1: y" ]
      unlines ["‘x’ finds:",
               "  2 entities with priority 1 or less: e1 e2",
               "  3 entities with priority 2 or less: e1 e2 e3",
               "",
               "‘y’ finds:",
               "  3 entities with priority 2 or less: e3 e1 e2" ]
        @=? warnings
  , testCase "Warn when an entity can't be found using only ASCII" $ do
      (_, warnings) <- testReadRulesAndWarnings $ T.unlines [
        "bad1 = 1: x€y",     -- bad because non-ASCII
        "bad2 = 1: ä",       -- bad because ASCII only encodes lower 128 and not <256
        "okay1 = 1: ₮ ***",  -- okay because there's a good pattern available
        "okay2 = 1: ₹",      -- okay because there's a good pattern in another rule
        "",
        "okay2 = 1: rupee" ]
      unlines ["‘bad1’ can't be found using only ASCII; patterns that find it are: x€y",
               "",
               "‘bad2’ can't be found using only ASCII; patterns that find it are: ä" ]
        @=? warnings
  ]

testReadRules :: Text -> IO [Rule]
testReadRules rulesString =
  case readRuleFile rulesString of
    Left err -> do
      assertFailure (show err)
      error "test failed"
    Right (rules, []) ->
      return rules
    Right (_rules, warnings) -> do
      assertFailure (unparagraphs warnings)
      error "test failed"

testReadRulesAndWarnings :: Text -> IO ([Rule], String)
testReadRulesAndWarnings rulesString =
  case readRuleFile rulesString of
    Left err -> do
      assertFailure (show err)
      error "test failed"
    Right (rules, warnings) ->
      return (rules, unparagraphs warnings)

-- | Separate paragraphs with blank lines.
unparagraphs :: [String] -> String
unparagraphs =
  intercalate "\n" . map (++ "\n") .
  map (dropWhile (== '\n')) . map (dropWhileEnd (== '\n'))
