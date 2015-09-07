{-# LANGUAGE
RecordWildCards,
OverloadedStrings,
ScopedTypeVariables,
FlexibleContexts
  #-}


module Main (main) where


-- General
import Data.Maybe
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Data.Tuple
import Control.Applicative
import Control.Monad
-- Monads
import Control.Monad.Writer
-- Lenses
import Lens.Micro.GHC hiding (set)
-- Lists
import Data.List (permutations, union)
-- Text
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Char
-- Parsing
import Text.Parsec hiding ((<|>), many, optional)
-- Containers
import qualified Data.Map as M
import Data.Map (Map)
import GHC.Exts (fromList)
-- GUI
import Graphics.UI.Gtk
-- Clipboard
import System.Hclip
-- Files
import System.FilePath
import System.Directory
-- Data files
import Paths_bob


-- | A thing that we use for search (like “->”).
type Pattern = Text

-- | A thing that we search for (like “→”).
type Entity = Text

type RuleName = Text

type Mapping = Map Pattern Entity

-- | Inverted 'Mapping' (used for references).
type Entities = Map Entity [Pattern]

data Generator
  = Literal Pattern
  | AnyOf [Generator]
  | Permutation [Generator]
  | Reference Entity
  deriving (Show)

literalP :: WarnParser Pattern
literalP = T.pack <$> asum [
  some literalChar,
  inSingleQuotes (many quotedChar) ]
  where
    literalChar = satisfy $ \x ->
      or [isSymbol x, isPunctuation x, isAlphaNum x] &&
      x `notElem` ("\"'`()[]{}" :: String)
    quotedChar = asum [
      try (string "''") >> pure '\'',
      satisfy $ \x -> not $ or [isSpace x, x == '\''] ]

generatorP :: WarnParser Generator
generatorP = asum [
  Literal <$> literalP,
  AnyOf <$> inParens (generatorP `sepBy1` some (char ' ')),
  Permutation <$> inBraces (generatorP `sepBy1` some (char ' ')),
  Reference <$> inBackticks literalP ]

data Matcher
  = Zip Text Text (Maybe Generator)
  | ManyToOne Generator Entity
  deriving (Show)

evalGenerator :: Entities -> Generator -> Warn [Pattern]
evalGenerator _  (Literal x) = return [x]
evalGenerator es (AnyOf gs) = concat <$> mapM (evalGenerator es) gs
evalGenerator es (Permutation gs) = do
  ps :: [[Pattern]] <- mapM (evalGenerator es) gs
  return $ do
    perm :: [[Pattern]] <- permutations ps
    chosen :: [Pattern] <- sequence perm
    return (mconcat chosen)
evalGenerator es (Reference x) = case M.lookup x es of
  Nothing -> do
    warn (printf "‘%s’ was referenced but wasn't defined yet" (T.unpack x))
    return []
  Just pats -> return pats

evalMatcher :: Entities -> Matcher -> Warn Mapping
evalMatcher es (Zip lineA lineB mbGen) = do
  let as = T.chunksOf 1 lineA
      bs = T.chunksOf 1 lineB
  additions <- case mbGen of
    Nothing  -> return [""]
    Just gen -> evalGenerator es gen
  return $ fromList $ concat $ do
    (a, b) <- zip as bs
    addition <- additions
    return [(addition <> a, b), (a <> addition, b), (b, b)]
evalMatcher es (ManyToOne g y) = do
  pats <- evalGenerator es g
  return $ fromList $ zip (y:pats) (repeat y)

matcherP :: WarnParser Matcher
matcherP = asum [zipP, manyToOneP]
  where
    -- Literals/generators/etc can go on a new line, but then they have to be
    -- indented. 'breaker' is a parser which either parses -a newline + some
    -- indentation-, or -just some spaces-.
    breaker = try (newline >> some (char ' ')) <|> some (char ' ')
    zipP = do
      string "zip"
      lineA <- breaker *> literalP
      lineB <- breaker *> literalP
      when (T.length lineA /= T.length lineB) $
        fail "lengths of zipped rows don't match"
      mbGen <- optional (breaker *> generatorP)
      return (Zip lineA lineB mbGen)
    manyToOneP = do
      x <- literalP
      breaker *> string "="
      g <- many (breaker *> generatorP)
      return (ManyToOne (AnyOf g) x)

data Rule = Rule {
  ruleName    :: RuleName,
  ruleMapping :: Mapping }
  deriving (Show)

ruleP :: WarnParser Rule
ruleP = do
  name <- currentLine
  let warningHeader = printf "warnings in rule ‘%s’:" (T.unpack name) :: String
      nameWarnings = warnCensor $ \ws ->
        if null ws then [] else warningHeader : map ("  " ++) ws
  nameWarnings $ do
    matchers <- matcherP `endBy1` newline
    -- Evaluate all matchers, combining generated mappings as we go along and
    -- passing them to each evaluator (so that references could be resolved).
    let go _ [] = return []
        go patterns (matcher:rest) = do
          mapping <- evalMatcher patterns matcher
          let patterns' = M.unionWith union (invertMap mapping) patterns
          (mapping:) <$> go patterns' rest
    mappings <- warnLift $ go mempty matchers
    -- Find matches assigned to more than one thing (e.g. “<<” meaning both
    -- ‘↞’ and ‘↢’) – it's allowed in different rules, but not in the same
    -- rule.
    let combinedMapping = M.unionsWith union $
          over (each.each) (:[]) mappings
    for_ (M.toList combinedMapping) $ \(k, vs) ->
      when (length vs /= 1) $
        warnLift $ warn $
          printf "“%s” corresponds to more than 1 thing: %s"
                 (T.unpack k) (T.unpack (T.intercalate ", " vs))
    return $ Rule {
      ruleName    = name,
      ruleMapping = mconcat mappings }

rulesP :: WarnParser [Rule]
rulesP = ruleP `sepBy1` some newline

matchRule :: Pattern -> Rule -> Maybe (RuleName, Entity)
matchRule query Rule{..} = do
  result <- M.lookup query ruleMapping
  return (ruleName, result)

matchRules :: Pattern -> [Rule] -> [(RuleName, Entity)]
matchRules query = mapMaybe (matchRule query)

runGUI :: [Rule] -> IO ()
runGUI rules = do
  initGUI

  window <- windowNew
  window `on` objectDestroy $ mainQuit
  set window [
    windowTitle := ("Bob" :: Text),
    windowGravity := GravityCenter,
    windowWindowPosition := WinPosCenter,
    windowDefaultWidth := 200,
    windowDefaultHeight := 200 ]

  searchEntry <- entryNew

  -- create a new list model
  model <- listStoreNew []
  view <- treeViewNewWithModel model

  treeViewSetHeadersVisible view True

  -- add a couple columns
  colChar <- treeViewColumnNew
  colRule <- treeViewColumnNew

  treeViewColumnSetTitle colChar ("Character" :: Text)
  treeViewColumnSetTitle colRule ("Rule" :: Text)

  rendererChar <- cellRendererTextNew
  rendererRule <- cellRendererTextNew

  cellLayoutPackStart colChar rendererChar True
  cellLayoutPackStart colRule rendererRule True

  cellLayoutSetAttributes colChar rendererChar model $ \row ->
    [ cellText := snd row ]
  cellLayoutSetAttributes colRule rendererRule model $ \row ->
    [ cellText := fst row ]
  layout <- tableNew 2 1 False  -- 2 rows, 1 column, autostretching = off
  tableAttachDefaults layout searchEntry 0 1 0 1
  tableAttachDefaults layout view 0 1 1 2
  window `containerAdd` layout

  set layout [
    tableChildYOptions searchEntry := [Fill] ]

  treeViewAppendColumn view colChar
  treeViewAppendColumn view colRule

  searchEntry `on` editableChanged $ do
    query <- get searchEntry entryText
    listStoreClear model
    let matches = matchRules query rules
    for_ matches $ \(name, result) ->
      listStoreAppend model (name, result)
    unless (null matches) $
      treeViewSetCursor view [0] Nothing

  let getVariantsNumber = length <$> listStoreToList model

  let choose mbIndex = do
        variantsNumber <- getVariantsNumber
        unless (variantsNumber == 0) $ do
          row <- listStoreGetValue model (fromMaybe 0 mbIndex)
          setClipboard (T.unpack (snd row))
          widgetDestroy window

  view `on` rowActivated $ \path _ -> choose (listToMaybe path)

  let getSelectedRow = listToMaybe . fst <$> treeViewGetCursor view

  searchEntry `on` keyPressEvent $ do
    key <- T.unpack <$> eventKeyName
    mbIndex <- liftIO getSelectedRow
    variantsNumber <- liftIO getVariantsNumber
    let indexPrev Nothing  = if variantsNumber == 0 then [] else [0]
        indexPrev (Just i) = [max 0 (i-1)]
    let indexNext Nothing  = if variantsNumber == 0 then [] else [0]
        indexNext (Just i) = [min (variantsNumber-1) (i+1)]
    liftIO $ case key of
      "Up"   -> do treeViewSetCursor view (indexPrev mbIndex) Nothing
                   return True
      "Down" -> do treeViewSetCursor view (indexNext mbIndex) Nothing
                   return True
      _other -> return False

  searchEntry `on` entryActivated $
    choose =<< getSelectedRow

  widgetShowAll window
  mainGUI

main :: IO ()
main = do
  dataDir <- getDataDir
  ruleFiles <- filter ((== ".rules") . takeExtensions) <$>
               getDirectoryContents (dataDir </> "rules")
  rules <- fmap concat . for ruleFiles $ \ruleFile -> do
    let path = dataDir </> "rules" </> ruleFile
    res <- warnParse rulesP ruleFile <$> T.readFile path
    case res of
      Left err -> do
        putStrLn (show err)
        return []
      Right (rules, warnings) -> do
        mapM_ putStrLn warnings
        return rules
  runGUI rules

currentLine :: WarnParser Text
currentLine = asum [
  endOfLine >> pure "",
  do x  <- anyChar
     xs <- anyChar `manyTill` try (eof <|> void endOfLine)
     pure (T.pack (x:xs)) ]

type Warn a = Writer [String] a

warn :: String -> Warn ()
warn s = tell [s]

type WarnParser a = Parsec Text [String] a

warnLift :: Warn a -> WarnParser a
warnLift x = do
  let (a, w) = runWriter x
  updateState (++ w)
  return a

warnCensor :: ([String] -> [String]) -> WarnParser a -> WarnParser a
warnCensor f p = do
  old <- getState
  putState []
  a <- p
  new <- getState
  putState (old ++ f new)
  return a

warnParse :: WarnParser a -> SourceName -> Text -> Either ParseError (a, [String])
warnParse p src s = runParser p' [] src s
  where
    p' = liftA2 (,) p getState

invertMap :: Ord b => Map a b -> Map b [a]
invertMap = M.fromListWith (++) . over (each._2) (:[]) . map swap . M.toList

inParens, inBraces, inSingleQuotes, inBackticks
  :: WarnParser a -> WarnParser a
inParens       = between (char '(')  (char ')')
inBraces       = between (char '{')  (char '}')
inSingleQuotes = between (char '\'') (char '\'')
inBackticks    = between (char '`')  (char '`')
