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
    , matcherTests
    , generatorTests
    , warningTests
    ]

parsingTests :: TestTree
parsingTests = testGroup "parsing"
  [ testCase "Simplest rule can be parsed" $ do
      rules <- testReadRules "a = 1: b\n"
      [Rule Nothing [("b", [("a", Top 1)])]] @=? rules

  , testCase "2 rules" $ do
      rules <- testReadRules $ T.unlines [
        "a = 1: b",
        "# whatever",
        "",
        "# whatever",
        "x = X: y" ]
      [Rule Nothing [("b", [("a", Top 1)])],
       Rule Nothing [("y", [("x", Whatever)])]]
        @=? rules

  , testCase "Newline at the end of file isn't needed" $ do
      rules <- testReadRules "a = 1: b"
      [Rule Nothing [("b", [("a", Top 1)])]] @=? rules

  , testCase "A file can be empty" $ do
      rules <- testReadRules ""
      [] @=? rules

  , testCase "A file can contain just a comment" $ do
      rules <- testReadRules "# comment"
      [] @=? rules

  , testCase "Priority can't be 0" $ do
      err <- testReadRulesButFail "a = 0: b"
      unlines ["line 1, column 6:",
               "expecting rest of integer",
               "priority can't be 0" ]
        @=? err
  ]

behaviorTests :: TestTree
behaviorTests = testGroup "behavior"
  [ testCase "####-notes should work" $ do
      rules <- testReadRules $ T.unlines [
        "####  blah",
        "a = 1: b" ]
      [Rule (Just "blah") [("b", [("a", Top 1)])]] @=? rules

  , testCase "Later priorities should count, not earlier" $ do
      rules <- testReadRules $ T.unlines [
        "a = 1: one two",
        "    2: two" ]
      [Rule Nothing [("one", [("a", Top 1)]),
                     ("two", [("a", Top 2)])]]
        @=? rules

  , testCase "A matcher can generate several entities for a pattern" $ do
      rules <- testReadRules $ T.unlines [
        "zip Aa",
        "    Xx",
        "    2: {(A a) ()}" ]
      [Rule Nothing [("AA",[("X",Top 2)]),
                     ("Aa",[("X",Top 2),("x",Top 2)]),
                     ("aA",[("X",Top 2),("x",Top 2)]),
                     ("aa",[("x",Top 2)])]]
        @=? rules
  ]

matcherTests :: TestTree
matcherTests = testGroup "matchers"
  [ testCase "zip" $ do
      rules <- testReadRules $ T.unlines [
        "zip ab",
        "    AB",
        "    1: ()+",
        "    2: +()" ]
      [Rule Nothing [("a+",[("A",Top 1)]),
                     ("b+",[("B",Top 1)]),
                     ("+a",[("A",Top 2)]),
                     ("+b",[("B",Top 2)])]]
        @=? rules

  , testCase "many-to-one" $ do
      rules <- testReadRules $ T.unlines [
        "x = 1: a",
        "    2: b" ]
      [Rule Nothing [("a",[("x",Top 1)]),
                     ("b",[("x",Top 2)])]]
        @=? rules

  , testCase "order" $ do
      rules <- testReadRules $ T.unlines [
        "(x y) : a b" ]
      [Rule Nothing [("x",[("a",Top 1),
                           ("b",Top 2)]),
                     ("y",[("a",Top 1),
                           ("b",Top 2)])]]
        @=? rules
  ]

generatorTests :: TestTree
generatorTests = testGroup "generators"
  [ testCase "row of generators" $ do
      rules <- testReadRules $ T.unlines [
        "x = 1: a (b c)" ]
      [Rule Nothing [("a",[("x",Top 1)]),
                     ("b",[("x",Top 1)]),
                     ("c",[("x",Top 1)])]]
        @=? rules

  , testCase "sequence" $ do
      rules <- testReadRules $ T.unlines [
        "x = 1: +(a b){c d}-" ]
      [Rule Nothing [("+acd-",[("x",Top 1)]),
                     ("+adc-",[("x",Top 1)]),
                     ("+bcd-",[("x",Top 1)]),
                     ("+bdc-",[("x",Top 1)])]]
        @=? rules

  , testCase "literal" $ do
      rules <- testReadRules $ T.unlines [
        "x = 1: a 'bcd' '''' '()'" ]
      [Rule Nothing [("a"  ,[("x",Top 1)]),
                     ("bcd",[("x",Top 1)]),
                     ("'"  ,[("x",Top 1)]),
                     ("()" ,[("x",Top 1)])]]
        @=? rules

  , testCase "single generator" $ do
      rules <- testReadRules $ T.unlines [
        "x = 1: (x y)" ]
      [Rule Nothing [("x",[("x",Top 1)]),
                     ("y",[("x",Top 1)])]]
        @=? rules

  , testCase "variable" $ do
      rules <- testReadRules $ T.unlines [
        "zip a",
        "    A",
        "    1: ()" ]
      [Rule Nothing [("a",[("A",Top 1)])]]
        @=? rules

  , testCase "any-of" $ do
      rules <- testReadRules $ T.unlines [
        "x = 1: (a b)" ]
      [Rule Nothing [("a",[("x",Top 1)]),
                     ("b",[("x",Top 1)])]]
        @=? rules

  , testCase "permutation" $ do
      rules <- testReadRules $ T.unlines [
        "x = 1: {a b}" ]
      [Rule Nothing [("ab",[("x",Top 1)]),
                     ("ba",[("x",Top 1)])]]
        @=? rules

  , testCase "reference" $ do
      rules <- testReadRules $ T.unlines [
        "a = 1: a b",
        "",
        "x = 1: `a``'a'`" ]
      [Rule Nothing [("a",[("a",Top 1)]),
                     ("b",[("a",Top 1)])],
       Rule Nothing [("aa",[("x",Top 1)]),
                     ("ab",[("x",Top 1)]),
                     ("ba",[("x",Top 1)]),
                     ("bb",[("x",Top 1)])]]
        @=? rules
  ]

warningTests :: TestTree
warningTests = testGroup "warnings"
  [ testCase "Warn when arguments of 'zip' have unequal lengths" $ do
      (_, warnings) <- testReadRulesAndWarnings $ T.unlines [
        "zip abc",
        "    wxyz",
        "    1: ()" ]
      warnings @?= unlines [
        "warnings in rule at line 1, column 1:",
        "  lengths of zipped rows don't match" ]

  , testCase "Warn when undefined things are referenced" $ do
      (_, warnings) <- testReadRulesAndWarnings $ T.unlines [
        "a   : 1 2 3",
        "`e` : x y z" ]
      warnings @?= unlines [
        "warnings in rule at line 1, column 1:",
        "  ‘e’ was referenced but wasn't defined yet" ]

  , testCase "Warn when no value can be provided for a variable" $ do
      (_, warnings) <- testReadRulesAndWarnings $ T.unlines [
        "a  : 1 2 3",
        "() : x y z" ]
      warnings @?= unlines [
        "warnings in rule at line 1, column 1:",
        "  there's a variable in the rule but no value provided for it" ]

  , testCase "Warn when empty patterns are encountered" $ do
      (_, warnings) <- testReadRulesAndWarnings $ T.unlines [
        "a  : 1 2 3",
        "'' : x y z" ]
      warnings @?= unlines [
        "warnings in rule at line 1, column 1:",
        "  matcher #2 contains an empty pattern" ]

  , testCase "Warn when priorities aren't satisfied" $ do
      (_, warnings) <- testReadRulesAndWarnings $ T.unlines [
        "e1 = 1: x",
        "     2: y",
        "e2 = 1: x",
        "     2: y",
        "e3 = 2: x",
        "     1: y" ]
      warnings @?= unlines [
        "‘x’ finds:",
        "  2 entities with priority 1 or less: e1 e2",
        "  3 entities with priority 2 or less: e1 e2 e3",
        "",
        "‘y’ finds:",
        "  3 entities with priority 2 or less: e3 e1 e2" ]

  , testCase "Warn when an entity can't be found using only ASCII" $ do
      (_, warnings) <- testReadRulesAndWarnings $ T.unlines [
        "bad1 = 1: x€y",     -- bad (non-ASCII)
        "bad2 = 1: ä",       -- bad (non-ASCII too)
        "okay1 = 1: ₮ ***",  -- okay (there's a good pattern available)
        "okay2 = 1: ₹",      -- okay (there's a good pattern in another rule)
        "",
        "okay2 = 1: rupee" ]
      warnings @?= unlines [
        "‘bad1’ can't be found with ASCII; found by: x€y",
        "",
        "‘bad2’ can't be found with ASCII; found by: ä" ]
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

testReadRulesButFail :: Text -> IO String
testReadRulesButFail rulesString =
  case readRuleFile rulesString of
    Left err ->
      -- Errors don't have “\n” at the end but we often compare them
      -- against something generated with 'unlines', and 'unlines'
      -- always adds a newline, so let's add a newline too.
      return (show err ++ "\n")
    Right _ -> do
      assertFailure "there was no error"
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
