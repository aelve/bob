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

testFinds :: [Rule] -> Writer [([Pattern], Entity)] () -> TestTree
testFinds rules = testGroup "finds" . with (allFindIn rules)

testBestMatches :: [Rule] -> Writer [([Pattern], Entity)] () -> TestTree
testBestMatches rules = testGroup "best matches" . with (bestMatchesIn rules)

arrowsTests :: [Rule] -> TestTree
arrowsTests rules = testGroup "arrows"
  [ testGroup "ordinary arrows"
    [ testFinds rules $ do
        [">", "->", ">-"]      .= "→"
        ["<", "<-", "-<"]      .= "←"
        ["v", "V", "|v", "v|"] .= "↓"
        ["^", "|^", "^|"]      .= "↑"
        --
        ["←→", "<>", "→←", "><"]    .= "↔"
        ["↓↑", "^v", "^|v", "|v|^"] .= "↕"
        --
        ["\\^", "^\\", "\\←", "<-\\", "<\\"] .= "↖"
        ["\\v", "v\\", "\\→", "->\\", "\\>"] .= "↘"
        ["/^",  "^/",  "/→",  "->/",  "/>"]  .= "↗"
        ["/v",  "v/",  "/←",  "<-/",  "</"]  .= "↙"
    , testBestMatches rules $ do
        ["<"] .= "←"
        [">"] .= "→"
        ["v"] .= "↓"
        ["^"] .= "↑"
        --
        ["<->", "<-->"]            .= "↔"
        ["v^", "^v", "v|^", "^|v"] .= "↕"
    ]

  , testGroup "double arrows"
    [ testFinds rules $ do
        ["<=", "←=", "=<"]               .= "⇐"
        ["=>", "=→", ">="]               .= "⇒"
        ["=^", "^=", "|^|", "=>^", "^⇐"] .= "⇑"
        ["=v", "v=", "|v|", "=>v", "v⇐"] .= "⇓"
        ["<=>", "⇐⇒", "↔=", "=↔"]        .= "⇔"
    , testBestMatches rules $ do
        ["<=", "←="]                    .= "⇐"
        ["=>", "=→"]                    .= "⇒"
        ["=^", "^=", "|^|", "⇒^", "^⇐"] .= "⇑"
        ["=v", "v=", "|v|", "⇒v", "v⇐"] .= "⇓"
        ["⇐⇒", "↔=", "=↔", "<=>"]       .= "⇔"
    ]

  , testGroup "crossed arrows"
    [ testFinds rules $ do
        ["<-/", "←/", "/←", "</-", "</"]  .= "↚"
        ["/->", "/→", "→/", "-/>", "/>"]  .= "↛"
        ["<=/", "/<=", "⇐/", "/⇐", "</="] .= "⇍"
        ["/=>", "=>/", "/⇒", "⇒/", "=/>"] .= "⇏"
    , testBestMatches rules $ do
        ["<-/", "←/", "</-", "</"] .= "↚"
        ["/->", "/→", "-/>", "/>"] .= "↛"
        ["<=/", "</=", "⇐/"]       .= "⇍"
        ["/=>", "=/>", "/⇒"]       .= "⇏"
    ]

  , testGroup "2-headed arrows"
    [ testFinds rules $ do
        ["→→", "→>", ">>", "->>", "-»", "→»"] .= "↠"
        ["←←", "<←", "<<", "<<-", "«-", "«←"] .= "↞"
        ["^^", "↑↑"]                          .= "↟"
        ["vv", "↓↓"]                          .= "↡"
    , testBestMatches rules $ do
        ["→>", "->>", "-»"] .= "↠"
        ["<←", "<<-", "«-"] .= "↞"
        ["^^"]              .= "↟"
        ["vv"]              .= "↡"
    ]

  , testGroup "arrows with tail"
    [ testFinds rules $ do
        [">>", ">->", ">→", "→>"] .= "↣"
        ["<<", "<-<", "←<", "<←"] .= "↢"
    , testBestMatches rules $ do
        [">->", ">→"] .= "↣"
        ["<-<", "←<"] .= "↢"
    ]

  , testGroup "arrows with bar"
    [ testFinds rules $ do
        ["|→", "→|", "|->", "|>"]             .= "↦"
        ["←|", "|←", "<-|", "<|"]             .= "↤"
        ["↑_", "_^", "|^_"]                   .= "↥"
        ["↓_", "_v", "|v_", "vT", "vt", "TV"] .= "↧"
    , testBestMatches rules $ do
        ["|→", "→|", "|->", "|>"] .= "↦"
        ["←|", "|←", "<-|", "<|"] .= "↤"
        ["↑_", "_^", "|^_"]       .= "↥"
        ["↓_", "_v", "|v_", "vT"] .= "↧"
    ]

  , testGroup "paired arrows"
    [ testFinds rules $ do
        ["→←", "><", "-><-", "←→", "<>", "<-->"]                      .= "⇄"
        ["←→", "<>", "<-->", "→←", "><", "-><-"]                      .= "⇆"
        ["→→", ">>", "->->", "->>", "-->>", "=>>", "=»", "-->", "=>"] .= "⇉"
        ["←←", "<<", "<-<-", "<<-", "<<--", "<<=", "«=", "<--", "<="] .= "⇇"
        --
        ["|^v|", "^||v", "↑↓"]                     .= "⇅"
        ["|v^|", "v||^", "↓↑"]                     .= "⇵"
        ["↓↓", "vv", "VV", "v||v", "vv||", "||VV"] .= "⇊"
        ["↑↑", "^^", "^||^", "^^||", "||^^"]       .= "⇈"
    , testBestMatches rules $ do
        ["→←", "-><-"]                      .= "⇄"
        ["←→"]                              .= "⇆"
        ["→→", "->->", "=»", "=>>", "-->>"] .= "⇉"
        ["←←", "<-<-", "«=", "<<=", "<<--"] .= "⇇"
        --
        ["|^v|", "^||v", "↑↓"] .= "⇅"
        ["|v^|", "v||^", "↓↑"] .= "⇵"
        ["↓↓", "v||v", "vv||"] .= "⇊"
        ["↑↑", "^||^", "^^||"] .= "⇈"
    ]
  ]

main :: IO ()
main = do
  (rules, mbErrors) <- readRules
  defaultMain $ testGroup "tests"
    [ testCase "No warnings when loading rules" $
        mbErrors @?= []
    , arrowsTests rules
    ]

findsIn :: [Rule] -> Pattern -> Entity -> TestTree
findsIn rules pattern entity = testCase name $ found @? ""
  where
    name  = printf "‘%s’ finds ‘%s’" pattern entity
    found = entity `elem` map snd (matchAndSortRules pattern rules)

-- todo: reverse the order of patterns and entity
allFindIn :: [Rule] -> [Pattern] -> Entity -> TestTree
allFindIn rules patterns entity = testGroup name cases
  where
    name = printf "finding ‘%s’" entity
    cases = [findsIn rules pattern entity | pattern <- patterns]

doesNotFindIn :: [Rule] -> Pattern -> Entity -> TestTree
doesNotFindIn rules pattern entity = testCase name $ not found @? ""
  where
    name = printf "‘%s’ doesn't find ‘%s’" pattern entity
    found = entity `elem` map snd (matchAndSortRules pattern rules)

partialOrderIn :: [Rule] -> Pattern -> [Entity] -> TestTree
partialOrderIn rules pattern entities = testGroup name tests
  where
    name = printf "‘%s’ finds %s in order" pattern (listTexts entities)
    tests = map order (zip entities (tail entities))
    order (e1, e2) = do
      let nameOrder = printf "‘%s’ comes before ‘%s’" e1 e2
      let matches = map snd (matchAndSortRules pattern rules)
          mbIndex1 = elemIndex e1 matches
          mbIndex2 = elemIndex e2 matches
      testCase nameOrder $ do
        isJust mbIndex1 @? printf "it doesn't even find ‘%s’" e1
        isJust mbIndex2 @? printf "it doesn't even find ‘%s’" e2
        mbIndex1 < mbIndex2 @? ""

bestMatchesIn :: [Rule] -> [Pattern] -> Entity -> TestTree
bestMatchesIn rules patterns entity
  | [pattern] <- patterns = bestMatchIn rules pattern entity
  | otherwise             = testGroup name cases
  where
    name = printf "all of %s are best matches for ‘%s’"
                  (listTexts patterns) entity
    cases = [bestMatchIn rules pattern entity | pattern <- patterns]

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

listTexts :: [Text] -> Text
listTexts = T.unwords . map (\s -> "‘" <> s <> "’")

-- | This lets use 'printf' strings with 'Text' arguments.
instance PrintfArg Text where
  formatArg = formatString . T.unpack
