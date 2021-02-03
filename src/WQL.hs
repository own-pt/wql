-- reference: en.wikibooks.org/wiki/Haskell/ParseExps
-- reference: two-wrongs.com/parser-combinators-parsing-for-haskell-beginners.html

module WQL where 

import GHC.Unicode ( isSpace, isAlpha, isDigit, isAlphaNum )
import Control.Applicative
import Text.ParserCombinators.ReadP as RP

{- DATA TYPES FIRST DEFINITIONS -}

type Pattern = String
type Variable = String

data Predicate = Predicate
  { predvar   :: Maybe Variable
  , predmod   :: Maybe Char
  , predpred  :: Maybe Pattern
--  , predLemma :: Maybe Pattern
--  , predPOS   :: Maybe Char
--  , predSense :: Maybe Char
  , predargs  :: Maybe [Arg]
  } deriving Show

data Arg = Arg
  { rolepat :: Pattern
  , argvar  :: Maybe Variable
  } deriving Show

data PredExpr
  = Or PredExpr PredExpr
  | And PredExpr PredExpr
  | Not PredExpr
  | P Predicate  deriving Show

data Cons = Cons
  { low  :: Variable
  , high :: Variable
--  , op   :: Pattern
  } deriving Show

data WQL = WQL
  { predx :: PredExpr -- predication
  , hcons :: Maybe [Cons] -- hcons
  } deriving Show

{- PREDICATION PARSER -}

satisfySpaces :: Char -> ReadP Char
satisfySpaces c = do
  skipSpaces *> char c <* skipSpaces

skipSpaces1 :: ReadP String
skipSpaces1 = do
  munch1 isSpace

variable :: ReadP Variable
variable = do
  code <- satisfy isAlpha
  body <- munch isAlphaNum 
  return (code : body)

predVar :: ReadP Variable
predVar = do
  variable <* char ':'

rolePat :: ReadP Pattern
rolePat = do
  munch1 $ \c -> (isAlphaNum c || c == '*')

argument :: ReadP Arg
argument = do
  rolePat_ <- rolePat
  variable_ <- (fmap Just $ skipSpaces1 *> variable) <++ (return Nothing)
  return (Arg rolePat_ variable_)

argSeparator :: ReadP Char
argSeparator = satisfySpaces ','

arglist :: ReadP [Arg]
arglist = do
  char '['
  skipSpaces
  arglist_ <- sepBy argument argSeparator
  skipSpaces
  char ']'
  return arglist_

predPat :: ReadP String
predPat = do
  underlineAbst <- option "" $ string "_"
  lemma <- munch1 $ \c -> (not (elem c "?[]{}|!&_") && not (isSpace c))
  pos <- option "" (do
    underlinePos <- char '_'
    charPos <- satisfy $ \c -> elem c "nvajrscpqxud"
    return [underlinePos, charPos])
  sense <- option "" (do            -- revisit later
    underlineSense <- option "" $ string "_"
    sense <- munch1 $ \c -> (not (elem c "?[]{}|!&_") && not (isSpace c))
    return (underlineSense ++ sense))
  option "" $ string "_rel"
  return (underlineAbst ++ lemma ++ pos ++ sense)

modifier :: ReadP Char
modifier = satisfy $ \c -> elem c "+/="

predication1 :: ReadP PredExpr
predication1 = do
  predVar_ <- (fmap Just predVar) <++ (return Nothing)
  modifier_ <- (fmap Just modifier) <++ (return Nothing)
  predPat_ <- (fmap Just predPat)
  arglist_ <- (fmap Just arglist) <++ (return Nothing)
  return (P $ Predicate predVar_ modifier_ predPat_ arglist_)

predication2 :: ReadP PredExpr
predication2 = do
  predVar_ <- (fmap Just predVar) <++ (return Nothing)
  arglist_ <- (fmap Just arglist)
  return (P $ Predicate predVar_ Nothing Nothing arglist_)

predication :: ReadP PredExpr
predication = do
  predication1 <|> predication2
  
{- LOGIGAL OPERATORS -}

parExpr :: ReadP PredExpr
parExpr = do
  char '('
  skipSpaces
  expression <- predExpr
  skipSpaces
  char ')'
  return expression

notExpr :: ReadP PredExpr
notExpr = do
  char '!'
  skipSpaces
  expression <- predication <|> parExpr <|> notExpr
  return (Not expression)

-- NOTE: "con" stands for "conjunction"
conExpr :: ReadP PredExpr
conExpr = do
  expressionL <- predication <|> parExpr <|> notExpr
  skipSpaces1
  expressionR <- predication <|> parExpr <|> notExpr <|> conExpr
  return (And expressionL expressionR)

-- NOTE: "dis" stands for "disjunction"
disExpr :: ReadP PredExpr
disExpr = do
  expressionL <- predication <|> parExpr <|> notExpr <|> conExpr
  satisfySpaces '|'
  expressionR <- predication <|> parExpr <|> notExpr <|> conExpr <|> disExpr
  return (Or expressionL expressionR)

predExpr :: ReadP PredExpr
predExpr = do
  expression <- predication <|> parExpr <|> notExpr <|> conExpr <|> disExpr
  return expression

{- LIST OF HCONS -}

hconstraint :: ReadP Cons
hconstraint = do
  low <- variable
  skipSpaces *> string "=q" <* skipSpaces
  high <- variable
  return (Cons low high)

hconstraints :: ReadP [Cons]
hconstraints = do
  char '{'
  skipSpaces
  hconstraints_ <- sepBy hconstraint argSeparator
  skipSpaces
  char '}'
  return hconstraints_

{- COMBINING TO DEFINE A WQL -}

wql :: ReadP WQL
wql = do
  skipSpaces
  predication_ <- predExpr
  skipSpaces
  hconstraints_ <- (fmap Just hconstraints) <++ (return Nothing)
  skipSpaces
  return (WQL predication_ hconstraints_)

{- OPTIMIZATIONS -}
_pushNots :: PredExpr -> PredExpr
-- NOTE: simple operators
_pushNots (Not predx) = Not (_pushNots predx)
_pushNots (Or predl predr) = Or (_pushNots predl) (_pushNots predr)
_pushNots (And predl predr) = And (_pushNots predl) (_pushNots predr)
-- NOTE: not-composite operators
_pushNots (Not (Not p)) = _pushNots p
_pushNots (Not (Or predl predr)) = And (_pushNots (Not predl)) (_pushNots (Not predr))
_pushNots (Not (And predl predr)) = Or (_pushNots (Not predl)) (_pushNots (Not predr))
-- NOTE: every other case
_pushNots predx = predx
  
pushNots :: WQL -> WQL
pushNots (WQL predx hcons) = WQL (_pushNots predx) hcons
