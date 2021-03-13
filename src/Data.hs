-- defining our data types

module Data where

type Pattern = String
type Variable = String

data Predicate = Predicate
  { predtop   :: Bool
  , predvar   :: Maybe Variable
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

