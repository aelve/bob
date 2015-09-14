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

testFoundBy :: [Rule] -> Writer [(Entity, [Pattern])] () -> TestTree
testFoundBy rules = testGroup "finds" .
                    with (foundByIn rules)

testBestMatchedBy :: [Rule] -> Writer [(Entity, [Pattern])] () -> TestTree
testBestMatchedBy rules = testGroup "best matches" .
                          with (bestMatchedByIn rules)

arrowsTests :: [Rule] -> TestTree
arrowsTests rules = testGroup "arrows"
  [ testGroup "ordinary arrows"
    [ testFoundBy rules $ do
        "→" .= [">", "->", ">-"]
        "←" .= ["<", "<-", "-<"]
        "↓" .= ["v", "V", "|v", "v|"]
        "↑" .= ["^", "|^", "^|"]
        --
        "↔" .= ["←→", "<>", "→←", "><"]
        "↕" .= ["↓↑", "^v", "^|v", "|v|^"]
        --
        "↖" .= ["\\^", "^\\", "\\←", "<-\\", "<\\"]
        "↘" .= ["\\v", "v\\", "\\→", "->\\", "\\>"]
        "↗" .= ["/^",  "^/",  "/→",  "->/",  "/>"]
        "↙" .= ["/v",  "v/",  "/←",  "<-/",  "</"]
    , testBestMatchedBy rules $ do
        "←" .= ["<"]
        "→" .= [">"]
        "↓" .= ["v"]
        "↑" .= ["^"]
        --
        "↔" .= ["<->", "<-->"]
        "↕" .= ["v^", "^v", "v|^", "^|v"]
    ]

  , testGroup "double arrows"
    [ testFoundBy rules $ do
        "⇐" .= ["<=", "←=", "=<"]
        "⇒" .= ["=>", "=→", ">="]
        "⇑" .= ["=^", "^=", "|^|", "=>^", "^⇐"]
        "⇓" .= ["=v", "v=", "|v|", "=>v", "v⇐"]
        "⇔" .= ["<=>", "⇐⇒", "↔=", "=↔"]
    , testBestMatchedBy rules $ do
        "⇐" .= ["<=", "←="]
        "⇒" .= ["=>", "=→"]
        "⇑" .= ["=^", "^=", "|^|", "⇒^", "^⇐"]
        "⇓" .= ["=v", "v=", "|v|", "⇒v", "v⇐"]
        "⇔" .= ["⇐⇒", "↔=", "=↔", "<=>"]
    ]

  , testGroup "crossed arrows"
    [ testFoundBy rules $ do
        "↚" .= ["<-/", "←/", "/←", "</-", "</"]
        "↛" .= ["/->", "/→", "→/", "-/>", "/>"]
        "⇍" .= ["<=/", "/<=", "⇐/", "/⇐", "</="]
        "⇏" .= ["/=>", "=>/", "/⇒", "⇒/", "=/>"]
    , testBestMatchedBy rules $ do
        "↚" .= ["<-/", "←/", "</-", "</"]
        "↛" .= ["/->", "/→", "-/>", "/>"]
        "⇍" .= ["<=/", "</=", "⇐/"]
        "⇏" .= ["/=>", "=/>", "/⇒"]
    ]

  , testGroup "2-headed arrows"
    [ testFoundBy rules $ do
        "↠" .= ["→→", "→>", ">>", "->>", "-»", "→»"]
        "↞" .= ["←←", "<←", "<<", "<<-", "«-", "«←"]
        "↟" .= ["^^", "↑↑"]
        "↡" .= ["vv", "↓↓"]
    , testBestMatchedBy rules $ do
        "↠" .= ["→>", "->>", "-»"]
        "↞" .= ["<←", "<<-", "«-"]
        "↟" .= ["^^"]
        "↡" .= ["vv"]
    ]

  , testGroup "arrows with tail"
    [ testFoundBy rules $ do
        "↣" .= [">>", ">->", ">→", "→>"]
        "↢" .= ["<<", "<-<", "←<", "<←"]
    , testBestMatchedBy rules $ do
        "↣" .= [">->", ">→"]
        "↢" .= ["<-<", "←<"]
    ]

  , testGroup "arrows with bar"
    [ testFoundBy rules $ do
        "↦" .= ["|→", "→|", "|->", "|>"]
        "↤" .= ["←|", "|←", "<-|", "<|"]
        "↥" .= ["↑_", "_^", "|^_"]
        "↧" .= ["↓_", "_v", "|v_", "vT", "vt", "TV"]
    , testBestMatchedBy rules $ do
        "↦" .= ["|→", "→|", "|->", "|>"]
        "↤" .= ["←|", "|←", "<-|", "<|"]
        "↥" .= ["↑_", "_^", "|^_"]
        "↧" .= ["↓_", "_v", "|v_", "vT"]
    ]

  , testGroup "paired arrows"
    [ testFoundBy rules $ do
        "⇄" .= ["→←", "><", "-><-", "←→", "<>", "<-->"]
        "⇆" .= ["←→", "<>", "<-->", "→←", "><", "-><-"]
        "⇉" .= ["→→", ">>", "->->", "->>", "-->>", "=>>", "=»", "-->", "=>"]
        "⇇" .= ["←←", "<<", "<-<-", "<<-", "<<--", "<<=", "«=", "<--", "<="]
        --
        "⇅" .= ["|^v|", "^||v", "↑↓"]
        "⇵" .= ["|v^|", "v||^", "↓↑"]
        "⇊" .= ["↓↓", "vv", "VV", "v||v", "vv||", "||VV"]
        "⇈" .= ["↑↑", "^^", "^||^", "^^||", "||^^"]
    , testBestMatchedBy rules $ do
        "⇄" .= ["→←", "-><-"]
        "⇆" .= ["←→"]
        "⇉" .= ["→→", "->->", "=»", "=>>", "-->>"]
        "⇇" .= ["←←", "<-<-", "«=", "<<=", "<<--"]
        --
        "⇅" .= ["|^v|", "^||v", "↑↓"]
        "⇵" .= ["|v^|", "v||^", "↓↑"]
        "⇊" .= ["↓↓", "v||v", "vv||"]
        "⇈" .= ["↑↑", "^||^", "^^||"]
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

doesNotFindIn :: [Rule] -> Pattern -> Entity -> TestTree
doesNotFindIn rules pattern entity = testCase name $ not found @? ""
  where
    name = printf "‘%s’ doesn't find ‘%s’" pattern entity
    found = entity `elem` map snd (matchAndSortRules pattern rules)

foundByIn :: [Rule] -> Entity -> [Pattern] -> TestTree
foundByIn rules entity patterns = testGroup name cases
  where
    name = printf "finding ‘%s’" entity
    cases = [findsIn rules pattern entity | pattern <- patterns]

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

bestMatchedByIn :: [Rule] -> Entity -> [Pattern] -> TestTree
bestMatchedByIn rules entity patterns
  | [pattern] <- patterns = bestMatchIn rules pattern entity
  | otherwise             = testGroup name cases
  where
    name = printf "all of %s are best matches for ‘%s’"
                  (listTexts patterns) entity
    cases = [bestMatchIn rules pattern entity | pattern <- patterns]

listTexts :: [Text] -> Text
listTexts = T.unwords . map (\s -> "‘" <> s <> "’")

-- | This lets use 'printf' strings with 'Text' arguments.
instance PrintfArg Text where
  formatArg = formatString . T.unpack
