{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- * Terms parsers

module CoALP.UI.Parser where

import CoALP
import CoALP.UI.Printer ()  -- import only the type class instances

import Control.DeepSeq
import Control.Monad ( void )
import Text.Parsec
import Text.Parsec.Prim (runP)
import Data.Functor.Identity
import Control.Applicative ( (<$>), (<*) )
import qualified Data.HashSet as HashSet
import Data.HashMap.Lazy ( HashMap )
import qualified Data.HashMap.Lazy as HashMap

data TermParserSt = TPS {tpsVars :: HashMap String Int, tpsNext :: Int}

instance NFData TermParserSt

-- | These instances are only marginally useful. Consider rewriting the
-- combinatory version in a cleaner way in the ReadPrec monad. The precedence
-- argument can be used to indicate the next available number for a fresh
-- variable. The assignment of variables to variable names can be kept in a
-- state monad.
instance Read Term1 where
  readsPrec p s = do
    return (fromStringSt onlyTerm    (TPS HashMap.empty p) s, "")

instance Read Clause1 where
  readsPrec p s = do
    return (fromStringSt onlyClause  (TPS HashMap.empty p) s, "")

instance Read Program1 where
  readsPrec p s = do
    return (fromStringSt onlyProgram (TPS HashMap.empty p) s, "")

instance Read Goal1 where
  readsPrec p s = do
    return (fromStringSt onlyGoal    (TPS HashMap.empty p) s, "")

instance Read ModeAssoc where
  readsPrec _ s = do
    return (fromStringSt onlyModeAssocs tpsInit s, "")


tpsInit :: TermParserSt
tpsInit = TPS HashMap.empty 0

-- | Empties the known variables, e.g., to start parsing the next implicitly
-- quantified formula.
forgetVars :: TermParser ()
forgetVars = putState . TPS HashMap.empty . tpsNext =<< getState

type TermParser = ParsecT String TermParserSt Identity

symbolP :: TermParser Char
symbolP = oneOf "!#$%&|*+-/<=>?@^_~"

identRest :: TermParser String
identRest = many (letter <|> symbolP <|> digit)

sep :: TermParser ()
sep = skipMany1 space

conId :: TermParser Ident
conId = do
  first <- lower <|> symbolP <|> digit
  rest <- identRest
  spaces
  return $ first:rest

con :: TermParser Term1
con = conId >>= return . flip Fun []

var :: TermParser Term1
var = do
  first <- upper
  rest <- identRest
  let v = first:rest
  st <- getState
  let (vs, nx) = (tpsVars st, tpsNext st)
      (upd, x) = case HashMap.lookup v vs of
        Nothing -> (putState (TPS (HashMap.insert v nx vs) (nx+1)), nx)
        Just x0 -> (return (), x0)
  upd
  spaces
  return $ Var x

parenOpen, parenClose :: TermParser ()
parenOpen  = char '(' >> spaces
parenClose = char ')' >> spaces

args :: TermParser [Term1]
args = between parenOpen parenClose $ (term <* spaces) `sepBy` comma

app :: TermParser Term1
app = do c <- conId; a <- args; return $ Fun c a

term :: TermParser Term1
term = try app <|> con <|> var

onlyTerm :: TermParser Term1
onlyTerm = term <* eof

from :: TermParser ()
from = string ":-" >> spaces

{-
typesign :: TermParser ()
typesign = string "::" >> spaces
-}

period :: TermParser ()
period = char '.' >> spaces

comma :: TermParser ()
comma = char ',' >> spaces

body ::  TermParser [Term1]
body =
  from >> spaces >>
  ((term <* spaces) `sepBy` comma) <* period

clause :: TermParser Clause1
clause = do
  h <- term
  spaces
  b <- try (period >> return []) <|> body
  return $ h :- b

onlyClause :: TermParser Clause1
onlyClause = clause <* eof

program :: TermParser Program1
program = spaces >> Pr <$> clause `sepEndBy` forgetVars

onlyProgram :: TermParser Program1
onlyProgram = program <* eof

onlyProgramSt :: TermParser (Program1, TermParserSt)
onlyProgramSt = do
  pr <- onlyProgram
  st <- getState
  return (pr, st)

goal :: TermParser Goal1
goal = do
  spaces >> from
  b <- (term <* spaces) `sepBy` comma
  period
  return $ Go b

onlyGoal :: TermParser Goal1
onlyGoal = goal <* eof

onlyGoalSt :: TermParser (Goal1, TermParserSt)
onlyGoalSt = do
  g <- onlyGoal
  st <- getState
  return (g, st)

-- | The parsing engine with the user state of type 'TermParserSt', starting
-- from the initial state.
termParse :: (Stream s Identity t) => Parsec s TermParserSt a ->
             SourceName -> s -> Either ParseError a
termParse p = runP p tpsInit

-- | The parsing engine with the user state of type 'TermParserSt', starting
-- from a given state.
termParseSt :: (Stream s Identity t) => Parsec s TermParserSt a ->
               TermParserSt -> SourceName -> s -> Either ParseError a
termParseSt = runP

termParseStCases :: TermParser a -> TermParserSt -> SourceName -> String -> a
termParseStCases pf tps lab txt =
  case termParseSt pf tps lab txt of
    Left e  -> error $ show e
    Right r -> r

termParseCases :: TermParser a -> SourceName -> String -> a
termParseCases pf = termParseStCases pf tpsInit

termParseFileStCases :: TermParser a -> TermParserSt -> SourceName ->
                        String -> IO a
termParseFileStCases pf tps lab fileName = do
  s <- readFile fileName
  case termParseSt pf tps lab s of
    Left e  -> print e >> fail "parse error"
    Right r -> return r

termParseFileCases :: TermParser a -> SourceName -> String -> IO a
termParseFileCases pf = termParseFileStCases pf tpsInit

fromStringSt :: TermParser a -> TermParserSt -> String -> a
fromStringSt f tps = termParseStCases f tps "fromStringSt"

programFromString :: String -> Program1
programFromString = termParseCases onlyProgram "program"

programFromFile :: String -> IO Program1
programFromFile = termParseFileCases onlyProgram "program"

input :: TermParser Direction
input = char '+' >> return Input

output :: TermParser Direction
output = char '-' >> return Output

direction :: TermParser Direction
direction = input <|> output

eager :: TermParser Order
eager = char '!' >> return Eager

lazy :: TermParser Order
lazy = char '?' >> return Lazy

order :: TermParser Order
order = eager <|> lazy

mode :: TermParser Mode
mode = do
  d <- direction
  o <- order
  return $ M d o

modes ::  TermParser [Mode]
modes =
  (between parenOpen parenClose $ (mode <* spaces) `sepBy` comma) <* try spaces

modeAssoc :: TermParser ModeAssoc
modeAssoc = do
  ident <- conId
  modes >>= return . ModeAssoc . HashMap.singleton ident . HashSet.singleton

{-
onlyModeAssoc :: TermParser ModeAssoc
onlyModeAssoc = modeAssoc <* eof
-}

onlyModeAssocs :: TermParser ModeAssoc
onlyModeAssocs = many1 modeAssoc <* eof >>= return . unionModeAssoc

modeAssocsFromString :: String -> ModeAssoc
modeAssocsFromString = termParseCases onlyModeAssocs "modes"

modeAssocsFromFile :: String -> IO ModeAssoc
modeAssocsFromFile file = do
  str <- readFile file
  case termParse onlyModeAssocs "modes" str of
    Left e  -> print e >> fail "parse error"
    Right r -> return r

-- | Top-level syntactic items present in input text.
data Item = ItemClause    Clause1
          | ItemGoal      Goal1
          | ItemModeAssoc ModeAssoc
          | ItemComment   ()            -- ^ forgets comments
          deriving (Show, Eq)

comment :: TermParser ()
comment = char '%' >> void (anyChar `manyTill`
                            try (void (newline >> spaces) <|> eof))

-- | Unordered top-level syntactic items.
items :: TermParser [Item]
items =
  many1
  (
    clause    `as` ItemClause    <|>
    goal      `as` ItemGoal      <|>
    modeAssoc `as` ItemModeAssoc <|>
    comment   `as` ItemComment   <|>
    unexpected "Unexpected item in bagging area :)"
  )
  where
    f `as` g = try $ f >>= return . g

itemsString :: String -> [Item]
itemsString = termParseCases items "items"

itemsFile :: String -> IO [Item]
itemsFile = termParseFileCases items "items"

-- | Categorisation function that applies after items have been retrieved from a
-- string or from a file.
groupItems :: [Item] -> ([Clause1], [Goal1], [ModeAssoc])
groupItems = foldr ins ([], [], [])
  where
    ins (ItemClause    c) (cs, gs, ms) = (c:cs, gs, ms)
    ins (ItemGoal      g) (cs, gs, ms) = (cs, g:gs, ms)
    ins (ItemModeAssoc m) (cs, gs, ms) = (cs, gs, m:ms)
    ins (ItemComment   _) old          = old

parseItemsFile :: String -> IO ([Clause1], [Goal1], [ModeAssoc])
parseItemsFile fn = itemsFile fn >>= return . groupItems

parseItems :: String -> ([Clause1], [Goal1], [ModeAssoc])
parseItems = groupItems . itemsString
