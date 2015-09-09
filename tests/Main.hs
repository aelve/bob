{-# LANGUAGE
OverloadedStrings
  #-}

module Main (main) where


-- Testing
import Test.Tasty
import Test.Tasty.HUnit
-- Text
import Text.Printf
import qualified Data.Text as T
-- Bob-specific
import Bob


main :: IO ()
main = do
  (rules, mbErrors) <- readRules
  let finds = findsIn rules
  defaultMain $ testGroup "Tests" [
    testCase "No warnings when loading rules" $
      mbErrors @?= Nothing,
    testGroup "Arrows" [
      "<=" `finds` "⇐",
      ">"  `finds` "→",
      "->" `finds` "→" ] ]

findsIn :: [Rule] -> Pattern -> Entity -> TestTree
findsIn rules pattern entity = testCase name $ found @? err
  where
    name  = printf "‘%s’ finds ‘%s’"
                   (T.unpack pattern) (T.unpack entity)
    found = entity `elem` map snd (matchRules pattern rules)
    err   = printf "‘%s’ doesn't find ‘%s’"
                   (T.unpack pattern) (T.unpack entity)
