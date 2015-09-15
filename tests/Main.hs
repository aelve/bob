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


main :: IO ()
main = do
  (rules, mbErrors) <- readRules
  defaultMain $ testGroup "tests"
    [ testCase "No warnings when loading rules" $
        mbErrors @?= []
    , arrowsTests rules
    ]

-- Here go some utility functions for tests.

{- |
This lets us use do notation to collect tests. See
<https://www.reddit.com/r/haskelltil/comments/3krrrr/enter_long_lists_with_do_notation_instead_of/> for a longer explanation.
-}
tests :: [Rule] -> Writer [[Rule] -> TestTree] a -> [TestTree]
tests rules ts = map ($ rules) (execWriter ts)

{- |
Test that an entity is found by all patterns; see 'foundByIn'.
-}
(<--) :: Entity -> [Pattern] -> Writer [[Rule] -> TestTree] ()
(<--) a b = tell [\rules -> foundByIn rules a b]

{- |
Test that an entity is the best match for all patterns; see 'bestMatchedByIn'.
-}
(<++) :: Entity -> [Pattern] -> Writer [[Rule] -> TestTree] ()
(<++) a b = tell [\rules -> bestMatchedByIn rules a b]

-- Here go tests themselves.

-- | Tests for @arrows.rules@.
arrowsTests :: [Rule] -> TestTree
arrowsTests rules = testGroup "arrows"
  [ testGroup "ordinary arrows" $ tests rules $ do
      "→" <++ [">"]
      "→" <-- ["->", ">-"]
      "←" <++ ["<"]
      "←" <-- ["<-", "-<"]
      "↓" <++ ["v", "|v", "v|"]
      "↓" <-- ["V"]
      "↑" <++ ["^", "|^", "^|"]
      "↑" <-- []
      --
      "↔" <++ ["<->", "<-->"]
      "↔" <-- ["←→", "<>", "→←", "><"]
      "↕" <++ ["v^", "^v", "v|^", "^|v"]
      "↕" <-- ["↓↑", "↑↓", "|v|^", "|^|v"]
      --
      "↖" <-- ["\\^", "^\\", "\\←", "<-\\", "<\\"]
      "↘" <-- ["\\v", "v\\", "\\→", "->\\", "\\>"]
      "↗" <-- ["/^",  "^/",  "/→",  "->/",  "/>"]
      "↙" <-- ["/v",  "v/",  "/←",  "<-/",  "</"]

  , testGroup "double arrows" $ tests rules $ do
      "⇐" <++ ["<=", "←="]
      "⇐" <-- ["=<"]
      "⇒" <++ ["=>", "=→"]
      "⇒" <-- [">="]
      "⇑" <++ ["=^", "^=", "|^|", "=>^", "^⇐"]
      "⇑" <-- []
      "⇓" <++ ["=v", "v=", "|v|", "=>v", "v⇐"]
      "⇓" <-- []
      "⇔" <++ ["<=>", "⇐⇒", "↔=", "=↔"]
      "⇔" <-- []

  , testGroup "crossed arrows" $ tests rules $ do
      "↚" <++ ["<-/", "←/", "/←", "</-", "</"]
      "↚" <-- ["/<-"]
      "↛" <++ ["/->", "/→", "→/", "-/>", "/>"]
      "↛" <-- ["->/"]
      "⇍" <++ ["<=/", "</=", "⇐/", "/⇐"]
      "⇍" <-- ["/<="]
      "⇏" <++ ["/=>", "=/>", "⇒/", "/⇒"]
      "⇏" <-- ["=>/"]

  , testGroup "2-headed arrows" $ tests rules $ do
      "↠" <++ ["→>", "->>", "-»"]
      "↠" <-- ["→→", ">>", "→»"]
      "↞" <++ ["<←", "<<-", "«-"]
      "↞" <-- ["←←", "<<", "«←"]
      "↟" <++ ["^^"]
      "↟" <-- ["↑↑"]
      "↡" <++ ["vv"]
      "↡" <-- ["↓↓"]

  , testGroup "arrows with tail" $ tests rules $ do
      "↣" <++ [">->", ">→"]
      "↣" <-- [">>", "→>"]
      "↢" <++ ["<-<", "←<"]
      "↢" <-- ["<<", "<←"]

  , testGroup "arrows with bar" $ tests rules $ do
      "↦" <++ ["|→", "→|", "|->", "|>"]
      "↦" <-- []
      "↤" <++ ["←|", "|←", "<-|", "<|"]
      "↤" <-- []
      "↥" <++ ["↑_", "_^", "|^_"]
      "↥" <-- []
      "↧" <++ ["↓_", "_v", "|v_", "vT"]
      "↧" <-- ["vt", "TV"]

  , testGroup "paired arrows" $ tests rules $ do
      "⇄" <++ ["→←", "-><-"]
      "⇄" <-- ["><", "←→", "<>", "<-->"]
      "⇆" <++ ["←→"]
      "⇆" <-- ["<>", "<-->", "→←", "><", "-><-"]
      "⇉" <++ ["→→", "->->", "=»", "=>>", "-->>"]
      "⇉" <-- [">>", "->>", "-->", "=>"]
      "⇇" <++ ["←←", "<-<-", "«=", "<<=", "<<--"]
      "⇇" <-- ["<<", "<<-", "<--", "<="]
      --
      "⇅" <++ ["|^v|", "^||v", "↑↓"]
      "⇅" <-- []
      "⇵" <++ ["|v^|", "v||^", "↓↑"]
      "⇵" <-- []
      "⇊" <++ ["↓↓", "v||v", "vv||"]
      "⇊" <-- ["vv", "VV", "||VV"]
      "⇈" <++ ["↑↑", "^||^", "^^||", "||^^"]
      "⇈" <-- ["^^"]
  ]

{- |
Test that a pattern finds an entity (e.g. “vv” finds “↡”, but doesn't find “ø”).
-}
findsIn :: [Rule] -> Pattern -> Entity -> TestTree
findsIn rules pattern entity = testCase name $ found @? ""
  where
    name  = printf "‘%s’ finds ‘%s’" pattern entity
    found = entity `elem` map snd (matchAndSortRules pattern rules)

{- |
Test that a pattern doesn't find an entity (see 'findsIn').
-}
doesNotFindIn :: [Rule] -> Pattern -> Entity -> TestTree
doesNotFindIn rules pattern entity = testCase name $ not found @? ""
  where
    name = printf "‘%s’ doesn't find ‘%s’" pattern entity
    found = entity `elem` map snd (matchAndSortRules pattern rules)

{- |
A version of 'findsIn' that does the test for many patterns at once (e.g. test that both “vv” and “↓↓” find “↡”).
-}
foundByIn :: [Rule] -> Entity -> [Pattern] -> TestTree
foundByIn rules entity patterns = testGroup name cases
  where
    name = printf "finding ‘%s’" entity
    cases = [findsIn rules pattern entity | pattern <- patterns]

{- |
Test that an entity is a best match for a pattern (i.e. “vv” may find both “↡” and “⇊”, but it gives preference to “↡”).
-}
bestMatchIn :: [Rule] -> Pattern -> Entity -> TestTree
bestMatchIn rules pattern entity = testCase name $ do
  let matches = map snd (matchAndSortRules pattern rules)
      surpassed = takeWhile (/= entity) matches
      found = entity `elem` matches
  found @? printf "it doesn't even find ‘%s’" entity
  null surpassed @? if length surpassed == 1
    then printf "%s comes before ‘%s’" (listTexts surpassed) entity
    else printf "%s come before ‘%s’"  (listTexts surpassed) entity
  where
    name = printf "‘%s’ finds ‘%s’ as 1st result" pattern entity

{- |
A version of 'bestMatchIn' that does the test for many patterns at once.
-}
bestMatchedByIn :: [Rule] -> Entity -> [Pattern] -> TestTree
bestMatchedByIn rules entity patterns
  | [pattern] <- patterns = bestMatchIn rules pattern entity
  | otherwise             = testGroup name cases
  where
    name = printf "all of %s are best matches for ‘%s’"
                  (listTexts patterns) entity
    cases = [bestMatchIn rules pattern entity | pattern <- patterns]

{- |
Test that a pattern has some order of preference for entities it finds (it might find other entities as well, but given ones have to come in the order they are specified).
-}
partialOrderIn :: [Rule] -> Pattern -> [Entity] -> TestTree
partialOrderIn rules pattern entities = testGroup name cases
  where
    name = printf "‘%s’ finds %s in order" pattern (listTexts entities)
    cases = map order (zip entities (tail entities))
    order (e1, e2) = do
      let nameOrder = printf "‘%s’ comes before ‘%s’" e1 e2
      let matches = map snd (matchAndSortRules pattern rules)
          mbIndex1 = elemIndex e1 matches
          mbIndex2 = elemIndex e2 matches
      testCase nameOrder $ do
        isJust mbIndex1 @? printf "it doesn't even find ‘%s’" e1
        isJust mbIndex2 @? printf "it doesn't even find ‘%s’" e2
        mbIndex1 < mbIndex2 @? ""

{- |
>>> listTexts ["a", "b", "c"]
"‘a’ ‘b’ ‘c’"
-}
listTexts :: [Text] -> Text
listTexts = T.unwords . map (\s -> "‘" <> s <> "’")

-- | This lets use 'printf' strings with 'Text' arguments.
instance PrintfArg Text where
  formatArg = formatString . T.unpack
