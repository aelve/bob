{-# LANGUAGE
OverloadedStrings
  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Main where


-- General
import Data.Maybe
-- Lists
import Data.List (elemIndex)
-- Monads
import Control.Monad.Writer
-- Text
import Text.Printf
import qualified Data.Text as T
import Data.Text (Text)
-- Testing
import Test.Tasty
import Test.Tasty.HUnit
-- Bob-specific
import Bob


with :: (a -> b -> c) -> Writer [(a, b)] () -> [c]
with f = map (uncurry f) . execWriter

(.=) :: a -> b -> Writer [(a, b)] ()
(.=) a b = tell [(a, b)]


main :: IO ()
main = do
  (rules, mbErrors) <- readRules
  let allFind = allFindIn rules
      partialOrder = partialOrderIn rules

  defaultMain $ testGroup "tests" [
    testCase "No warnings when loading rules" $
      mbErrors @?= Nothing
    ,

    testGroup "arrows" [
      testGroup "finds" $ with allFind $ do
        [">", "->"]                 .= "→"
        ["<", "<-"]                 .= "←"
        ["←→", "<>"]                .= "↔"
        ["v", "|v", "v|"]           .= "↓"
        ["^", "|^", "^|"]           .= "↑"
        ["↓↑", "^v", "^|v", "|v|^"] .= "↕"
        ["\\^", "^\\"]              .= "↖"
        ["/^", "^/"]                .= "↗"
        ["\\v", "v\\"]              .= "↘"
        ["/v", "v/"]                .= "↙"
        ["<=", "←="]                .= "⇐"
        ["=>", "=→"]                .= "⇒"
        ["=^", "|^|", "=>^", "^⇐"]  .= "⇑"
        ["=v", "|v|", "=>v", "v⇐"]  .= "⇓"
      ,
      testGroup "partial orders" $ with partialOrder $ do
        ">=" .= ["≥", "⇒"]
      ]
    ]

findsIn :: [Rule] -> Pattern -> Entity -> TestTree
findsIn rules pattern entity = testCase name $ found @? ""
  where
    name  = printf "‘%s’ finds ‘%s’" pattern entity
    found = entity `elem` map snd (matchRules pattern rules)

allFindIn :: [Rule] -> [Pattern] -> Entity -> TestTree
allFindIn rules patterns entity = testGroup name cases
  where
    name = printf "finding ‘%s’" entity
    cases = [findsIn rules pattern entity | pattern <- patterns]

doesNotFindIn :: [Rule] -> Pattern -> Entity -> TestTree
doesNotFindIn rules pattern entity = testCase name $ not found @? ""
  where
    name = printf "‘%s’ doesn't find ‘%s’" pattern entity
    found = entity `elem` map snd (matchRules pattern rules)

partialOrderIn :: [Rule] -> Pattern -> [Entity] -> TestTree
partialOrderIn rules pattern entities = testGroup name tests
  where
    name = printf "‘%s’ finds %s in order" pattern
                  (T.unwords (map (\s -> "‘" <> s <> "’") entities))
    tests = map order (zip entities (tail entities))
    order (e1, e2) = do
      let nameOrder = printf "‘%s’ comes before ‘%s’" e1 e2
      let matches = map snd (matchRules pattern rules)
          mbIndex1 = elemIndex e1 matches
          mbIndex2 = elemIndex e2 matches
      testCase nameOrder $ do
        isJust mbIndex1 @? printf "doesn't even find ‘%s’" e1
        isJust mbIndex2 @? printf "doesn't even find ‘%s’" e2
        mbIndex1 < mbIndex2 @? ""

-- | This lets use 'printf' strings with 'Text' arguments.
instance PrintfArg Text where
  formatArg = formatString . T.unpack
