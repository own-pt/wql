import Text.ParserCombinators.ReadP
import WQL

-- Leaf stands for sparql expression
-- of a single predication

data Triple = Triple
  { surj :: String
  , pred :: String
  , objc :: String
  } deriving Show

type Leaf = [Triple]

data Graph
  = Leaf
  | Graph Graph
  | Union Graph Graph
  | Filter_Not_Exists Graph
  deriving Show

data Sparql = Sparql
  { prefixes      :: [String] -- namespaces
  , patternGraph  :: Graph    -- where clause
--  , configuration  :: [String] -- etc
  } deriving Show

--wql_to_sparql :: WQL.WQL -> Sparql
--wql_to_sparql w = 


{- pendencies -}
