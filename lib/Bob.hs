{-# LANGUAGE
RecordWildCards,
ScopedTypeVariables,
OverloadedStrings
  #-}

module Bob
(
  Pattern,
  Entity,
  RuleName,
  Rule(..),
  readRules,
  matchRules,
  matchAndSortRules,
)
where


-- General
import Data.Maybe
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Control.Applicative
import Control.Monad
-- Monads
import Control.Monad.Writer
-- Lenses
import Lens.Micro.GHC
-- Lists
import Data.List (permutations, union)
-- Sorting
import Data.List (sortOn)
import Data.Ord (Down(..))
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
-- Files
import System.FilePath
import System.Directory
-- Data files
import Paths_bob


-- | A thing that we use for search (like “->”).
type Pattern = Text

-- | A thing that we search for (like “→”).
type Entity = Text

-- | How well a pattern corresponds to an entity; for instance, “\>=” would
-- have bad fitness for “⇒”, but “=\>” would have good fitness for “⇒”. The
-- higher the number is, the better.
type Fitness = Int

type RuleName = Text

-- | Entities corresponding to a pattern (like “->” leads to “→”). There can
-- be several entities corresponding to a pattern even inside a single rule.
type EntitiesMap = Map Pattern [(Entity, Fitness)]

-- | All patterns that an entity corresponds to (like “→” leads to “->”, “>”).
type PatternsMap = Map Entity [(Pattern, Fitness)]

toPatternsMap :: EntitiesMap -> PatternsMap
toPatternsMap m = M.fromListWith (++) $ do
  (pattern, entities) <- M.toList m
  (entity, fitness) <- entities
  return (entity, [(pattern, fitness)])

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

fitnessP :: WarnParser Fitness
fitnessP = read <$> some digit

generatorP :: WarnParser Generator
generatorP = do
  let singleGenerator = asum [
        Literal <$> literalP,
        AnyOf <$> inParens (generatorP `sepBy1` someSpaces),
        Permutation <$> inBraces (generatorP `sepBy1` someSpaces),
        Reference <$> inBackticks literalP ]
  gens <- some singleGenerator
  return $ case gens of
    [gen] -> gen
    _     -> Sequence gens

type Generators = [(Generator, Fitness)]

data Matcher
  = Zip Text Text Generators
  | ManyToOne Generators Entity
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
  Just pats -> return (map fst pats)

evalGenerators :: PatternsMap -> Generators -> Warn [(Pattern, Fitness)]
evalGenerators psm gens = do
  groups :: [([Pattern], Fitness)] <- (each._1) (evalGenerator psm) gens
  -- Now groups have to be expanded, and patterns from later groups should
  -- replace earlier patterns. See https://github.com/aelve/bob/issues/47.
  let pats :: [(Pattern, Fitness)]
      pats = [(p, f) | (ps, f) <- groups, p <- ps]
  -- To leave only the last occurrence of each pattern it's enough to convert
  -- the list to a Map and back, because that's how Map's fromList works.
  return $ M.toList (M.fromList pats)

evalMatcher
  :: PatternsMap  -- ^ Already existing patterns
  -> Matcher      -- ^ Matcher to evaluate
  -- | Generated entities (not in 'EntitiesMap' because for a matcher it's
  -- guaranteed that each pattern would correspond to a unique entity).
  -> Warn (Map Pattern (Entity, Fitness))
evalMatcher psm (Zip lineA lineB gens) = do
  let as = T.chunksOf 1 lineA
      bs = T.chunksOf 1 lineB
  additions <- evalGenerators psm gens
  return $ M.fromList $ concat $ do
    (a, b) <- zip as bs
    (pattern, fitness) <- if null additions then [("", 10)] else additions
    return [(pattern <> a, (b, fitness)), (a <> pattern, (b, fitness))]
evalMatcher psm (ManyToOne gens entity) = do
  patterns <- evalGenerators psm gens
  return $ M.fromList $ do
    (pattern, fitness) <- (entity, 10) : patterns
    return (pattern, (entity, fitness))

generatorLineP :: WarnParser (Generator, Fitness)
generatorLineP = do
  fitness <- fitnessP <* char ':'
  someSpaces
  gens <- generatorP `sepBy1` someSpaces
  return (AnyOf gens, fitness)

matcherP :: WarnParser Matcher
matcherP = asum [zipP, manyToOneP]
  where
    nextLine = try (newline >> someSpaces)
    zipP = do
      string "zip" <* someSpaces
      lineA <- literalP <* nextLine
      lineB <- literalP <* nextLine
      when (T.length lineA /= T.length lineB) $
        warnLift $ warn "lengths of zipped rows don't match"
      gens <- generatorLineP `sepBy1` nextLine
      return (Zip lineA lineB gens)
    manyToOneP = do
      x <- literalP <* someSpaces
      char '=' <* someSpaces
      gens <- generatorLineP `sepBy1` nextLine
      return (ManyToOne gens x)

data Rule = Rule {
  ruleName     :: RuleName,
  ruleEntities :: EntitiesMap }
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
           -> EntitiesMap    -- ^ all generated entities so far
           -> [Matcher]      -- ^ matchers left to process
           -> Warn EntitiesMap
        go _psm entitiesMap [] = return entitiesMap
        go  psm entitiesMap (matcher:rest) = do
          entities <- evalMatcher psm matcher
          let entityMap = fmap (:[]) entities
          go (M.unionWith union psm (toPatternsMap entityMap))
             (M.unionWith union entityMap entitiesMap)
             rest
    entitiesMap <- warnLift $ go scope mempty matchers
    -- Return the rule.
    let rule = Rule {
          ruleName     = name,
          ruleEntities = entitiesMap }
    return rule

ruleFileP :: WarnParser [Rule]
ruleFileP = do
  rule1 <- ruleP mempty
  (rule1:) <$> go (toPatternsMap (ruleEntities rule1))
  where
    go psm = asum [
      -- Either there is a new rule...
      do some newline
         rule <- ruleP psm
         let psm' = psm <> toPatternsMap (ruleEntities rule)
         (rule:) <$> go psm',
      -- ...or there isn't.
      pure [] ]

matchRule :: Pattern -> Rule -> [((RuleName, Entity), Fitness)]
matchRule query Rule{..} = do
  (entity, fitness) <- M.findWithDefault [] query ruleEntities
  return ((ruleName, entity), fitness)

matchRules :: Pattern -> [Rule] -> [((RuleName, Entity), Fitness)]
matchRules query = concatMap (matchRule query)

matchAndSortRules :: Pattern -> [Rule] -> [(RuleName, Entity)]
matchAndSortRules query =
  map fst . sortOn (Down . snd) . concatMap (matchRule query)

-- | Returns rules and warnings\/parsing errors (if there were any).
readRules :: IO ([Rule], [String])
readRules = do
  dataDir <- getDataDir
  ruleFiles <- filter ((== ".rules") . takeExtensions) <$>
               getDirectoryContents (dataDir </> "rules")
  results <- for ruleFiles $ \ruleFile -> do
    let path = dataDir </> "rules" </> ruleFile
    res <- warnParse ruleFileP ruleFile <$> T.readFile path
    return $ case res of
      Left err -> ([], [show err])
      Right (rules, warnings) -> (rules, warnings)
  let rules  = concat (map fst results)
      errors = filter (not . null) (map snd results)
  return (rules, map unlines errors)

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

inParens, inBraces, inSingleQuotes, inBackticks
  :: WarnParser a -> WarnParser a
inParens       = between (char '(')  (char ')')
inBraces       = between (char '{')  (char '}')
inSingleQuotes = between (char '\'') (char '\'')
inBackticks    = between (char '`')  (char '`')

someSpaces :: WarnParser ()
someSpaces = void (some (char ' '))
