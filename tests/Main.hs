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
      "→" <++ T.words ">"
      "→" <-- T.words "->  >-"
      "←" <++ T.words "<"
      "←" <-- T.words "<-  -<"
      "↓" <++ T.words "v  |v  v|"
      "↓" <-- T.words "V"
      "↑" <++ T.words "^  |^  ^|"
      --
      "↔" <++ T.words "<->  <-->"
      "↔" <-- T.words "←→  <>  →←  ><"
      "↕" <++ T.words "v^  ^v  v|^  ^|v"
      "↕" <-- T.words "↓↑  ↑↓  |v|^  |^|v"
      --
      "↖" <-- T.words "\\^  ^\\  \\←  <-\\  <\\"
      "↘" <-- T.words "\\v  v\\  \\→  ->\\  \\>"
      "↗" <-- T.words "/^   ^/    /→  ->/   />"
      "↙" <-- T.words "/v   v/    /←  <-/   </"

  , testGroup "double arrows" $ tests rules $ do
      "⇐" <++ T.words "<=  ←="
      "⇐" <-- T.words "=<"
      "⇒" <++ T.words "=>  =→"
      "⇒" <-- T.words ">="
      "⇔" <++ T.words "<=>  ⇐⇒  ↔=  =↔"
      --
      "⇑" <++ T.words "=^  ^=  |^|  =>^  ^⇐"
      "⇓" <++ T.words "=v  v=  |v|  =>v  v⇐"

  , testGroup "crossed arrows" $ tests rules $ do
      "↚" <++ T.words "<-/  ←/  /←  </-  </"
      "↚" <-- T.words "/<-"
      "↛" <++ T.words "/->  /→  →/  -/>  />"
      "↛" <-- T.words "->/"
      --
      "⇍" <++ T.words "<=/  </=  ⇐/  /⇐"
      "⇍" <-- T.words "/<="
      "⇏" <++ T.words "/=>  =/>  ⇒/  /⇒"
      "⇏" <-- T.words "=>/"

  , testGroup "2-headed arrows" $ tests rules $ do
      "↠" <++ T.words "→>  ->>  -»"
      "↠" <-- T.words "→→  >>  →»"
      "↞" <++ T.words "<←  <<-  «-"
      "↞" <-- T.words "←←  <<  «←"
      --
      "↟" <++ T.words "^^"
      "↟" <-- T.words "↑↑"
      "↡" <++ T.words "vv"
      "↡" <-- T.words "↓↓"

  , testGroup "arrows with tail" $ tests rules $ do
      "↣" <++ T.words ">->  >→"
      "↣" <-- T.words ">>  →>"
      "↢" <++ T.words "<-<  ←<"
      "↢" <-- T.words "<<  <←"

  , testGroup "arrows with bar" $ tests rules $ do
      "↦" <++ T.words "|→  →|  |->  |>"
      "↤" <++ T.words "←|  |←  <-|  <|"
      "↥" <++ T.words "↑_  _^  |^_"
      "↧" <++ T.words "↓_  _v  |v_  vT"
      "↧" <-- T.words "vt  TV"

  , testGroup "paired arrows" $ tests rules $ do
      "⇄" <++ T.words "→←  -><-"
      "⇄" <-- T.words "><  ←→  <>  <-->"
      "⇆" <++ T.words "←→"
      "⇆" <-- T.words "<>  <-->  →←  ><  -><-"
      --
      "⇉" <++ T.words "→→  ->->  =»  =>>  -->>"
      "⇉" <-- T.words ">>  ->>  -->  =>"
      "⇇" <++ T.words "←←  <-<-  «=  <<=  <<--"
      "⇇" <-- T.words "<<  <<-  <--  <="
      --
      "⇅" <++ T.words "|^v|  ^||v  ↑↓"
      "⇵" <++ T.words "|v^|  v||^  ↓↑"
      --
      "⇊" <++ T.words "↓↓  v||v  vv||"
      "⇊" <-- T.words "vv  VV  ||VV"
      "⇈" <++ T.words "↑↑  ^||^  ^^||  ||^^"
      "⇈" <-- T.words "^^"
  ]

-- | Tests for @currencies.rules@.
currenciesTests :: [Rule] -> TestTree
currenciesTests rules = testGroup "currencies" $ tests rules $ do
  "¤" <++ T.words "currency  ox  OX"
  "¤" <-- T.words "Ox  oX"
  "¢" <++ T.words "cent  penny  c|"
  "¢" <-- T.words "c  C|  c/  C/  |c  1/100  0.01  .01"
  "₥" <++ T.words "mill  mille  mil  m/"
  "₥" <-- T.words "m  /m  1/1000  0.001  .001"
  "$" <++ T.words "dollar  s|  S|  s||  S||  usd  USD"
  "$" <-- T.words "s  S  s/  S/"
  "£" <++ T.words "pound  lb  l-  L-  lf  Lf  gbp  GBP"
  "£" <-- T.words "l  L  #"
  "€" <++ T.words "euro  e=  E=  e-  E-  c=  C=  eur  EUR"
  "€" <-- T.words "e  E  =e  =E  c--  C--"
  "₡" <++ T.words "colon  colón  c//  C//  crc  CRC  svc  SVC"
  "₡" <-- T.words "c  C  c/  C/  //c  //C"
  "₦" <++ T.words "naira  n=  N=  ngn  NGN"
  "₦" <-- T.words "n  N  =n  =N"
  "₧" <++ T.words "peseta  Pts  pts  esp  ESP"
  "₧" <-- T.words "Pt  pt"
  "₵" <++ T.words "cedi  C|  ghs  GHS"
  "₵" <-- T.words "c  C  c| |c  |C  c/  C/"
  "₱" <++ T.words "peso  p=  P=  php  PHP"
  "₱" <-- T.words "p  P  p-  P-  =p  =P"
  "₴" <++ T.words "hryvnia  s=  S=  г=  uah  UAH"
  "₴" <-- T.words "s  S  г  s--  г-  hrn"
  "₮" <++ T.words "togrog  tögrög  tugrik  T//  mnt  MNT"
  "₮" <-- T.words "t  T  t/  T/  t//"
  "₽" <++ T.words "ruble  r-  R-  rub  RUB"
  "₽" <-- T.words "p  P  p-  P-  p=  P=  r  R"
  -- these ‘Р’s are Cyrillic
  "₽" <++ T.words "р=  Р=  р-  Р-"
  "₽" <-- T.words "р  Р  =р  =Р"
  "₺" <++ T.words "lira  t=  t//  L=  L//  trl  TRL"
  "₺" <-- T.words "t  l  L  t/  L/  t-  L-"
  "₫" <++ T.words "dong  d-_  vnd  VND"
  "₫" <-- T.words "d  d-  d_  -d  _d  -d_  _d-"
  "₭" <++ T.words "kip  K-  k-  lak  LAK"
  "₭" <-- T.words "k  K  -k  -K"
  "₩" <++ T.words "won  w=  W=  krw  KRW  kpw  KPW"
  "₩" <-- T.words "w  W  w--  W--  w-  W-  w__  W__  __w  __W  w_  W_"
  "₪" <++ T.words "shekel  sheqel  nis  ils  ILS"
  "₨" <++ T.words "rupee  Rs  rs"
  "₹" <++ T.words "inr  INR"
  "₹" <-- T.words "rupee"
  "₳" <++ T.words "austral  A=  ara  ARA"
  "₳" <-- T.words "a  A  a=  a--  A--  A-"
  "¥" <++ T.words "yuan  yen  Y=  cny  CNY  jpy  JPY"
  "¥" <-- T.words "y  Y  y--  Y--  y-  Y-  =y  =Y"
  "฿" <++ T.words "baht  B|  thb  THB"
  "฿" <-- T.words "b  B  |B  b/  B/"
  "₲" <++ T.words "guarani  guaraní  G|  pyg  PYG"
  "₲" <-- T.words "g  G  |G  g/  G/"
  "ƒ" <++ T.words "florin  gulden  guilder  fl"
  "ƒ" <-- T.words "f"

-- | Tests for @diacritics.rules@.
diacriticsTests :: [Rule] -> TestTree
diacriticsTests rules = testGroup "diacritics"
  [ testGroup "diaeresis" $ tests rules $ do
      testRows
        "AaEeHhIiOotUuWwXxYy"
        "ÄäËëḦḧÏïÖöẗÜüẄẅẌẍŸÿ"
        $ \x y -> do y <++ [x <> ":", ":" <> x, x <> "\"", x <> ".."]
                     y <-- [x]
  -- We want people to be able to find e.g. “ḛ” quickly if they want to, so
  -- it's a rule of sorts that “e~” means “ẽ” and “~e” means “ḛ”.
  , testGroup "tilde" $ tests rules $ do
      testRows
        "ANOanoIiUuEeYyVv"
        "ÃÑÕãñõĨĩŨũẼẽỸỹṼṽ"
        $ \x y -> do y <++ [x <> "~"]
                     y <-- [x, "~" <> x]
  , testGroup "tilde below" $ tests rules $ do
      testRows
        "EeIiUu"
        "ḚḛḬḭṴṵ"
        $ \x y -> do y <++ ["~" <> x]
                     y <-- [x, x <> "~"]
  ]
  where testRows a b f = zipWithM f (T.chunksOf 1 a) (T.chunksOf 1 b)

{-

grave accent
zip AaEeIiNnOoUuWwYy
    ÀàÈèÌìǸǹÒòÙùẀẁỲỳ
    0: '`' \ ''

stroke
zip AaCcEeLlOo
    ȺⱥȻȼɆɇŁłØø
    0: / - ''
zip BbDdGgHhIiJjKkPpRrTtYyZz
    ɃƀĐđǤǥĦħƗɨɈɉꝀꝁⱣᵽɌɍŦŧɎɏƵƶ
    0: - / ''

acute accent
zip AaCcEeGgIiKkLlMmNnOoPpRrSsUuWwYyZz
    ÁáĆćÉéǴǵÍíḰḱĹĺḾḿŃńÓóṔṕŔŕŚśÚúẂẃÝýŹź
    0: '''' / , ''

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
  "↯" <++ T.words "hp  HP  harry  Harry  potter  Potter"

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
