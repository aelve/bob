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

-- | Entity corresponding to a pattern (like “→” corresponds to “->”).
type EntityMap = Map Pattern Entity

-- | All patterns that an entity corresponds to (like “->” and “>” to “→”).
type PatternsMap = Map Entity [Pattern]

patternsUnion :: PatternsMap -> PatternsMap -> PatternsMap
patternsUnion = M.unionWith union

data Generator
  = Literal Pattern
  | AnyOf [Generator]
  | Sequence [Generator]
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
generatorP = do
  let singleGenerator = asum [
        Literal <$> literalP,
        AnyOf <$> inParens (generatorP `sepBy1` some (char ' ')),
        Permutation <$> inBraces (generatorP `sepBy1` some (char ' ')),
        Reference <$> inBackticks literalP ]
  gens <- some singleGenerator
  return $ case gens of
    [gen] -> gen
    _     -> Sequence gens

data Matcher
  = Zip Text Text (Maybe Generator)
  | ManyToOne Generator Entity
  deriving (Show)

evalGenerator :: PatternsMap -> Generator -> Warn [Pattern]
evalGenerator _  (Literal x) = return [x]
evalGenerator psm (AnyOf gs) = concat <$> mapM (evalGenerator psm) gs
evalGenerator psm (Sequence gs) = do
  ps :: [[Pattern]] <- mapM (evalGenerator psm) gs
  return $ do
    chosen :: [Pattern] <- sequence ps
    return (mconcat chosen)
evalGenerator psm (Permutation gs) = do
  ps :: [[Pattern]] <- mapM (evalGenerator psm) gs
  return $ do
    perm :: [[Pattern]] <- permutations ps
    chosen :: [Pattern] <- sequence perm
    return (mconcat chosen)
evalGenerator em (Reference x) = case M.lookup x em of
  Nothing -> do
    warn (printf "‘%s’ was referenced but wasn't defined yet" (T.unpack x))
    return []
  Just pats -> return pats

evalMatcher
  :: PatternsMap     -- ^ Already existing patterns
  -> Matcher         -- ^ Matcher to evaluate
  -> Warn EntityMap  -- ^ Generated entities
evalMatcher psm (Zip lineA lineB mbGen) = do
  let as = T.chunksOf 1 lineA
      bs = T.chunksOf 1 lineB
  additions <- case mbGen of
    Nothing  -> return [""]
    Just gen -> evalGenerator psm gen
  return $ fromList $ concat $ do
    (a, b) <- zip as bs
    addition <- additions
    return [(addition <> a, b), (a <> addition, b), (b, b)]
evalMatcher psm (ManyToOne g y) = do
  pats <- evalGenerator psm g
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
  ruleName     :: RuleName,
  ruleEntities :: EntityMap }
  deriving (Show)

ruleP :: PatternsMap -> WarnParser Rule
ruleP scope = do
  name <- currentLine
  let header = printf "warnings in rule ‘%s’:" (T.unpack name)
  groupWarnings header $ do
    matchers <- matcherP `endBy1` newline
    -- Evaluate all matchers, combining generated patterns as we go along and
    -- passing them to each evaluator (so that references could be resolved).
    let go :: PatternsMap    -- ^ all entities in scope
           -> [EntityMap]    -- ^ all generated entity maps so far
           -> [Matcher]      -- ^ matchers left to process
           -> Warn [EntityMap]
        go _psm entityMaps [] = return entityMaps
        go  psm entityMaps (matcher:rest) = do
          entityMap <- evalMatcher psm matcher
          let patternsMap = invertMap entityMap
          go (psm `patternsUnion` patternsMap) (entityMap:entityMaps) rest
    entityMaps <- warnLift $ go scope [] matchers
    -- Find patterns assigned to more than one entity (e.g. “<<” meaning both
    -- ‘↞’ and ‘↢’) – it's allowed in different rules, but not in the same
    -- rule.
    let combinedEntities :: Map Pattern [Entity]
        combinedEntities = M.unionsWith union $
          over (each.each) (:[]) entityMaps
    -- TODO: find a better name for combinedEntities.
    for_ (M.toList combinedEntities) $ \(k, vs) ->
      when (length vs /= 1) $
        warnLift $ warn $
          printf "“%s” corresponds to more than 1 thing: %s"
                 (T.unpack k) (T.unpack (T.intercalate ", " vs))
    -- Return the rule.
    let rule = Rule {
          ruleName     = name,
          ruleEntities = mconcat entityMaps }
    return rule

ruleFileP :: WarnParser [Rule]
ruleFileP = do
  rule1 <- ruleP mempty
  (rule1:) <$> go (invertMap (ruleEntities rule1))
  where
    go psm = asum [
      -- Either there is a new rule...
      do some newline
         rule <- ruleP psm
         let psm' = psm <> invertMap (ruleEntities rule)
         (rule:) <$> go psm',
      -- ...or there isn't.
      pure [] ]

matchRule :: Pattern -> Rule -> Maybe (RuleName, Entity)
matchRule query Rule{..} = do
  result <- M.lookup query ruleEntities
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
    res <- warnParse ruleFileP ruleFile <$> T.readFile path
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

groupWarnings :: String -> WarnParser a -> WarnParser a
groupWarnings title p = do
  old <- getState
  putState []
  a <- p
  new <- getState
  if null new
    then putState old
    else putState (old ++ title : map ("  " ++) new)
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
