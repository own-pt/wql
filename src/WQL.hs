-- reference: en.wikibooks.org/wiki/Haskell/ParseExps
-- reference: two-wrongs.com/parser-combinators-parsing-for-haskell-beginners.html

module WQL where 

import GHC.Unicode ( isSpace, isAlpha, isDigit, isAlphaNum )
import Control.Applicative
import Text.ParserCombinators.ReadP as RP
import Data
import Data.Text as T

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
  munch1 $ \c -> isAlphaNum c || c == '*'

argument :: ReadP Arg
argument = do
  rolePat_ <- rolePat
  variable_ <- fmap Just (skipSpaces1 *> variable) <++ return Nothing
  return $ Arg rolePat_ variable_

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

lemma = do
  munch1 $ \c -> c `notElem` "?[]{}|!&_()^" && not (isSpace c)
pos = do
  underlinePos <- char '_'
  charPos <- satisfy $ \c -> c `elem` "nvajrscpqxud"
  return [underlinePos, charPos]
sense = do
  underlineSense <- option "" $ string "_"
  sense <- munch1 $ \c -> notElem c "?[]{}|!&_()^" && not (isSpace c)
  return (underlineSense ++ sense)

predPat :: ReadP String
predPat = do
  under_ <- option "" $ string "_"
  lemma_ <- lemma
  pos_ <- option "" pos
  sense_ <- option "" sense
  rel_ <- option "" $ string "_rel"
  return (under_ ++ lemma_ ++ pos_ ++ sense_ ++ rel_)

predTop :: ReadP Bool
predTop = do
  predTop_ <- char '^'
  return $ predTop_ == '^'
  
modifier :: ReadP Char
modifier = satisfy $ \c -> c `elem` "+/="

predication1 :: ReadP PredExpr
predication1 = do
  predTop_ <- predTop <++ return False
  predVar_ <- fmap Just predVar <++ return Nothing
  modifier_ <- fmap Just modifier <++ return Nothing
  predPat_ <- fmap Just predPat
  arglist_ <- fmap Just arglist <++ return Nothing
  return (P $ Predicate predTop_ predVar_ modifier_ predPat_ arglist_)

predication2 :: ReadP PredExpr
predication2 = do
  predVar_ <- fmap Just predVar <++ return Nothing
  arglist_ <- fmap Just arglist
  return $ P $ Predicate False predVar_ Nothing Nothing arglist_

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
  high <- variable
  skipSpaces *> string "=q" <* skipSpaces
  low <- variable
  return (Cons high low)

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
  hconstraints_ <- fmap Just hconstraints <++ return Nothing
  skipSpaces
  return (WQL predication_ hconstraints_)

{- OPTIMIZATIONS -}
_pushNots :: PredExpr -> PredExpr
-- NOTE: simple operators
_pushNots (Not predx) = Not (_pushNots predx)
_pushNots (Or predl predr) = Or (_pushNots predl) (_pushNots predr)
_pushNots (And predl predr) = And (_pushNots predl) (_pushNots predr)
-- NOTE: not-composite operators
-- _pushNots (Not (Not p)) = _pushNots p
-- _pushNots (Not (Or predl predr)) = And (_pushNots (Not predl)) (_pushNots (Not predr))
-- _pushNots (Not (And predl predr)) = Or (_pushNots (Not predl)) (_pushNots (Not predr))
-- NOTE: every other case
_pushNots predx = predx
  
pushNots :: WQL -> WQL
pushNots (WQL predx hcons) = WQL (_pushNots predx) hcons
