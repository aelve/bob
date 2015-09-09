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
      doesNotFind = doesNotFindIn rules
  defaultMain $ testGroup "Tests" [
    testCase "No warnings when loading rules" $
      mbErrors @?= Nothing,
    testGroup "Arrows" [
      testGroup "finds" [
        ">"  `finds` "→", "->" `finds` "→",
        "←→" `finds` "↔", "<>" `finds` "↔",
        "↑↓" `finds` "↕", "↓↑" `finds` "↕",
        "<=" `finds` "⇐", "←=" `finds` "⇐" ],
      testGroup "doesn't find" [
        "=<" `doesNotFind` "⇐", ">=" `doesNotFind` "⇒" ]
      ]
    ]

findsIn :: [Rule] -> Pattern -> Entity -> TestTree
findsIn rules pattern entity = testCase name $ found @? ""
  where
    name  = printf "‘%s’ finds ‘%s’"
                   (T.unpack pattern) (T.unpack entity)
    found = entity `elem` map snd (matchRules pattern rules)

doesNotFindIn :: [Rule] -> Pattern -> Entity -> TestTree
doesNotFindIn rules pattern entity = testCase name $ not found @? ""
  where
    name  = printf "‘%s’ doesn't find ‘%s’"
                   (T.unpack pattern) (T.unpack entity)
    found = entity `elem` map snd (matchRules pattern rules)
