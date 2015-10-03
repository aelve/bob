{-# LANGUAGE
OverloadedStrings,
FlexibleInstances
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
import Data.String
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
    , currenciesTests rules
    , diacriticsTests rules
    , weirdTests rules
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
found :: Entity -> [Pattern] -> Writer [[Rule] -> TestTree] ()
found e ps = tell [\rules -> foundByIn rules e ps]

{- |
Test that an entity is the best match for all patterns; see 'bestMatchedByIn'.
-}
best :: Entity -> [Pattern] -> Writer [[Rule] -> TestTree] ()
best e ps = tell [\rules -> bestMatchedByIn rules e ps]

{- |
Test that an entity is in top N matches; see 'wellMatchedByIn'.
-}
top :: Int -> Entity -> [Pattern] -> Writer [[Rule] -> TestTree] ()
top n e ps = tell [\rules -> wellMatchedByIn rules e n ps]

-- Here goes an instance that lets us encode lists of patterns as strings
-- (so, ["a", "b", "c"] becomes "a b c").

instance IsString [Pattern] where
  fromString = T.words . T.pack

-- Here go tests themselves.

-- | Tests for @arrows.rules@.
arrowsTests :: [Rule] -> TestTree
arrowsTests rules = testGroup "arrows"
  [ testGroup "ordinary arrows" $ tests rules $ do
      best   "→"  ">  ->"
      top 3  "→"  ">-"
      best   "←"  "<  <-"
      top 3  "←"  "-<"
      best   "↓"  "v  |v  v|"
      top 3  "↓"  "V"
      best   "↑"  "^  |^  ^|"
      --
      best   "↔"  "<->  <-->"
      top 2  "↔"  "←→  <>  →←  ><"
      best   "↕"  "v^  ^v  v|^   ^|v"
      top 2  "↕"  "↓↑  ↑↓  |v|^  |^|v"
      --
      top 2  "↖"  "\\^  ^\\  \\←  <-\\  <\\"
      top 2  "↘"  "\\v  v\\  \\→  ->\\  \\>"
      top 2  "↗"  "/^   ^/    /→  ->/   />"
      top 2  "↙"  "/v   v/    /←  <-/   </"

  , testGroup "double arrows" $ tests rules $ do
      best   "⇐"  "<=  ←="
      top 3  "⇐"  "=<"
      best   "⇒"  "=>  =→"
      top 3  "⇒"  ">="
      best   "⇔"  "<=>  ⇐⇒  ↔=  =↔"
      --
      best   "⇑"  "=^  ^=  |^|  =>^  ^⇐"
      best   "⇓"  "=v  v=  |v|  =>v  v⇐"

  , testGroup "crossed arrows" $ tests rules $ do
      best   "↚"  "<-/  ←/  /←  </-  </"
      top 2  "↚"  "/<-"
      best   "↛"  "/->  /→  →/  -/>  />"
      top 2  "↛"  "->/"
      --
      best   "⇍"  "<=/  </=  ⇐/  /⇐"
      top 2  "⇍"  "/<="
      best   "⇏"  "/=>  =/>  ⇒/  /⇒"
      top 2  "⇏"  "=>/"

  , testGroup "2-headed arrows" $ tests rules $ do
      best   "↠"  "→>  ->>  -»"
      top 2  "↠"  "→→  >>   →»"
      best   "↞"  "<←  <<-  «-"
      top 2  "↞"  "←←  <<   «←"
      --
      best   "↟"  "^^"
      top 2  "↟"  "↑↑"
      best   "↡"  "vv"
      top 2  "↡"  "↓↓"

  , testGroup "arrows with tail" $ tests rules $ do
      best   "↣"  ">->  >→"
      top 2  "↣"  ">>   →>"
      best   "↢"  "<-<  ←<"
      top 2  "↢"  "<<   <←"

  , testGroup "arrows with bar" $ tests rules $ do
      best   "↦"  "|→  →|  |->  |>"
      best   "↤"  "←|  |←  <-|  <|"
      best   "↥"  "↑_  _^  |^_"
      best   "↧"  "↓_  _v  |v_  vT"
      top 2  "↧"  "vt  TV"

  , testGroup "paired arrows" $ tests rules $ do
      best   "⇄"  "→←  -><-"
      top 2  "⇄"  "><"
      top 3  "⇄"  "←→  <>  <-->"
      best   "⇆"  "←→"
      top 2  "⇆"  "<>  <-->"
      top 3  "⇆"  "><  -><-  →←"
      --
      best   "⇉"  "→→  ->->  =»  =>>  -->>"
      top 2  "⇉"  "->>  -->"
      top 3  "⇉"  ">>  =>"
      best   "⇇"  "←←  <-<-  «=  <<=  <<--"
      top 2  "⇇"  "<<-  <--"
      top 3  "⇇"  "<<  <="
      --
      best   "⇅"  "↑↓  |^v|  ^||v"
      best   "⇵"  "↓↑  |v^|  v||^"
      --
      best   "⇊"  "↓↓  v||v  vv||"
      top 2  "⇊"  "vv  VV    ||VV"
      best   "⇈"  "↑↑  ^||^  ^^||  ||^^"
      top 2  "⇈"  "^^"
  ]

-- | Tests for @currencies.rules@.
currenciesTests :: [Rule] -> TestTree
currenciesTests rules = testGroup "currencies" $ tests rules $ do
  -- commonly used ones
  best   "$"  "s  S  dollar  s|  S|  s||  S||  s/  S/  usd  USD"
  best   "£"  "l  L  pound  lb  lf  Lf  gbp  GBP  l-  L-"
  top 2  "£"  "#"
  best   "€"  "e  E  euro  e-  E-  e=  E=  c=  C=  eur  EUR"
  top 2  "€"  "=e  =E  c--  C--"
  best   "¥"  "y  Y  yuan  yen  Y=  y=  Y-  y-  cny  CNY  jpy  JPY"
  top 2  "¥"  "y--  Y--  =y  =Y"
  best   "₪"  "shekel  sheqel  nis  ils  ILS"
  -- ruble
  best   "₽"  "r  R  ruble  rub  RUB"
  top 2  "₽"  "r-  R-  p-  P-"
  top 3  "₽"  "p  P  p=  P="
  best   "₽"  "р  Р  р=  Р=  р-  Р-"   -- these ‘Р’s are Cyrillic
  top 2  "₽"  "=р  =Р"                 -- and these
  -- rare currencies
  best   "₡"  "colon  colón  c//  C//  crc  CRC  svc  SVC"
  found  "₡"  "c  C  c/  C/  //c  //C"
  best   "₦"  "naira  n=  N=  ngn  NGN"
  found  "₦"  "n  N  =n  =N"
  best   "₧"  "peseta  Pts  pts  esp  ESP"
  found  "₧"  "Pt  pt"
  best   "₵"  "cedi  C|  ghs  GHS"
  found  "₵"  "c  C  c| |c  |C  c/  C/"
  best   "₱"  "peso  p=  P=  php  PHP"
  found  "₱"  "p  P  p-  P-  =p  =P"
  best   "₴"  "hryvnia  s=  S=  г=  uah  UAH"
  found  "₴"  "s  S  г  s--  г-  hrn"
  best   "₮"  "togrog  tögrög  tugrik  T//  mnt  MNT"
  found  "₮"  "t  T  t/  T/  t//"
  best   "₺"  "lira  t=  t//  L=  L//  trl  TRL"
  found  "₺"  "t  l  L  t/  L/  t-  L-"
  best   "₫"  "dong  d-_  vnd  VND"
  found  "₫"  "d  d-  d_  -d  _d  -d_  _d-"
  best   "₭"  "kip  lak  LAK"
  found  "₭"  "k  K  K-  k-  -k  -K"
  best   "₩"  "won  w=  W=  krw  KRW  kpw  KPW"
  found  "₩"  "w  W  w--  W--  w-  W-  w__  W__  __w  __W  w_  W_"
  best   "₨"  "rupee  Rs  rs"
  best   "₹"  "inr  INR"
  found  "₹"  "rupee"
  best   "₳"  "austral  A=  ara  ARA"
  found  "₳"  "a  A  a=  a--  A--  A-"
  best   "฿"  "baht  B|  thb  THB"
  found  "฿"  "b  B  |B  b/  B/"
  best   "₲"  "guarani  guaraní  G|  pyg  PYG"
  found  "₲"  "g  G  |G  g/  G/"
  best   "ƒ"  "florin  gulden  guilder  fl"
  found  "ƒ"  "f"
  -- not currencies
  best   "¤"  "currency  ox  OX  Ox  oX"
  best   "¢"  "cent  penny  c/  c|  c"
  top 2  "¢"  "C|  C/  |c  1/100  0.01  .01"
  best   "₥"  "mill  mille  mil  m/"
  top 2  "₥"  "/m  1/1000  0.001  .001"
  found  "₥"  "m"

-- | Tests for @diacritics.rules@.
diacriticsTests :: [Rule] -> TestTree
diacriticsTests rules = testGroup "diacritics"
  [ testGroup "diaeresis" $ tests rules $ do
      testRows
        "AaEeHhIiOotUuWwXxYy"
        "ÄäËëḦḧÏïÖöẗÜüẄẅẌẍŸÿ"
        $ \x y -> do best  y [x <> ":", ":" <> x,
                              x <> "\"", "\"" <> x,
                              x <> "..", ".." <> x]
                     found y [x]
  -- We want people to be able to find e.g. “ḛ” quickly if they want to, so
  -- it's a rule of sorts that “e~” means “ẽ” and “~e” means “ḛ”.
  , testGroup "tilde" $ tests rules $ do
      testRows
        "ANOanoIiUuEeYyVv"
        "ÃÑÕãñõĨĩŨũẼẽỸỹṼṽ"
        $ \x y -> do best  y [x <> "~"]
                     top 2 y ["~" <> x]
                     found y [x]
  , testGroup "tilde below" $ tests rules $ do
      testRows
        "EeIiUu"
        "ḚḛḬḭṴṵ"
        $ \x y -> do best  y ["~" <> x]
                     top 2 y [x <> "~"]
                     found y [x]
  , testGroup "grave accent" $ tests rules $ do
      testRows
        "AaEeIiNnOoUuWwYy"
        "ÀàÈèÌìǸǹÒòÙùẀẁỲỳ"
        $ \x y -> do best  y ["`" <> x, x <> "`",
                              "\\" <> x, x <> "\\"]
                     found y [x]
  , testGroup "stroke" $ tests rules $ do
      testRows
        "AaCcEeLlOo"
        "ȺⱥȻȼɆɇŁłØø"
        $ \x y -> do best  y ["/" <> x]
                     top 2 y [x <> "/"]
                     top 3 y ["-" <> x, x <> "-"]
                     found y [x]
      testRows
        "BbDdGgHhIiJjKkPpRrTtYyZz"
        "ɃƀĐđǤǥĦħƗɨɈɉꝀꝁⱣᵽɌɍŦŧɎɏƵƶ"
        $ \x y -> do best  y ["-" <> x]
                     top 2 y [x <> "-"]
                     top 3 y ["/" <> x, x <> "/"]
                     found y [x]
  , testGroup "acute accent" $ tests rules $ do
      testRows
        "AaCcEeGgIiKkLlMmNnOoPpRrSsUuWwYyZz"
        "ÁáĆćÉéǴǵÍíḰḱĹĺḾḿŃńÓóṔṕŔŕŚśÚúẂẃÝýŹź"
        $ \x y -> do best  y ["'" <> x, x <> "'",
                              "," <> x, x <> ","]
                     top 3 y ["/" <> x, x <> "/"]
                     found y [x]
  ]
  where testRows a b f = zipWithM f (T.chunksOf 1 a) (T.chunksOf 1 b)

{-

breve
zip AaEeIiOoUu
    ĂăĔĕĬĭŎŏŬŭ
    0: U u '(' ')' ''

circumflex
zip AaCcEeGgHhIiJjOoSsUuWwYyZz
    ÂâĈĉÊêĜĝĤĥÎîĴĵÔôŜŝÛûŴŵŶŷẐẑ
    0: ^ /\ < > ''

caron
zip AaIiCcDdEeGgHhjKkLlNnOoRrSsTtUuZz
    ǍǎǏǐČčĎďĚěǦǧȞȟǰǨǩĽľŇňǑǒŘřŠšŤťǓǔŽž
    0: V v \/ < > ''
zip dtLl
    ďťĽľ
    0: ''''

bar
zip Ll
    Ƚƚ
    0: - ''

diagonal stroke
zip TtKkQqVv
    ȾⱦꝂꝃꝘꝙꝞꝟ
    0: / ''

double acute accent
zip OoUu
    ŐőŰű
    0: '"' // ,, = : ''

double grave accent
zip AaEeIiOoRrUu
    ȀȁȄȅȈȉȌȍȐȑȔȕ
    0: '``' \\ '"' = : ''

ring
zip AaUuwy
    ÅåŮůẘẙ
    0: o O 0 '()' ''

-}



-- | Tests for @weird.rules@.
weirdTests :: [Rule] -> TestTree
weirdTests rules = testGroup "weird rules" $ tests rules $ do
  best  "↯"  "hp  HP  harry  Harry  potter  Potter"

{- |
Test that a pattern finds an entity (e.g. “vv” finds “↡”, but doesn't find “ø”).
-}
findsIn :: [Rule] -> Pattern -> Entity -> TestTree
findsIn rules pattern entity = testCase name $ isFound @? ""
  where
    name    = printf "‘%s’ finds ‘%s’" pattern entity
    isFound = entity `elem` map snd (matchAndSortRules pattern rules)

{- |
Test that a pattern doesn't find an entity (see 'findsIn').
-}
doesNotFindIn :: [Rule] -> Pattern -> Entity -> TestTree
doesNotFindIn rules pattern entity = testCase name $ not isFound @? ""
  where
    name    = printf "‘%s’ doesn't find ‘%s’" pattern entity
    isFound = entity `elem` map snd (matchAndSortRules pattern rules)

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
      isFound = entity `elem` matches
  isFound @? printf "it doesn't even find ‘%s’" entity
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
A version of 'bestMatchIn' that checks that the match is in top N matches, as opposed to necessarily being the best match.
-}
goodMatchIn :: [Rule] -> Int -> Pattern -> Entity -> TestTree
goodMatchIn rules pos pattern entity = testCase name $ do
  let matches = map snd (matchAndSortRules pattern rules)
      surpassed = takeWhile (/= entity) matches
      isFound = entity `elem` matches
  isFound @? printf "it doesn't even find ‘%s’" entity
  length surpassed < pos @?
    printf "there are %d matches before ‘%s’: %s"
           (length surpassed) entity (listTexts surpassed)
  where
    name = printf "‘%s’ finds ‘%s’ in top %d matches" pattern entity pos

{- |
A version of 'goodMatchIn' that does the test for many patterns at once.
-}
wellMatchedByIn :: [Rule] -> Entity -> Int -> [Pattern] -> TestTree
wellMatchedByIn rules entity pos patterns
  | [pattern] <- patterns = goodMatchIn rules pos pattern entity
  | otherwise             = testGroup name cases
  where
    name = printf "all of %s are good matches for ‘%s’"
                  (listTexts patterns) entity
    cases = [goodMatchIn rules pos pattern entity | pattern <- patterns]

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
