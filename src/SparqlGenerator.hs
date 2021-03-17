{-# LANGUAGE OverloadedStrings #-}

module SparqlGenerator where

import GHC.Unicode ( isSpace, isAlpha, isDigit, isAlphaNum )
import Control.Applicative
import Text.ParserCombinators.ReadP as RP
import Data 
import Database.HSparql.QueryGenerator as QG
import qualified Data.Text as T
import qualified Data.Map as Map
import WQL
import Control.Monad
import Control.Monad.State
  
-- now the SPARQL generator        
mrs = prefix "mrs" (iriRef "http://www.delph-in.net/schema/mrs#")
erg = prefix "erg" (iriRef "http://www.delph-in.net/schema/erg#")
delph = prefix "delph" (iriRef "http://www.delph-in.net/schema/")
rdf = prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
rdfs = prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")
xsd = prefix "xsd" (iriRef "http://www.w3.org/2001/XMLSchema#")

type VariablesMap = Map.Map Data.Variable QG.Variable 
transformation :: PredExpr -> Query SelectQuery
transformation pred =
  do
    mrs <- mrs ; erg <- erg; delph <- delph; rdf <- rdf; rdfs <- rdfs; xsd <- xsd

    let prefixes = [mrs, erg, delph, rdf, rdfs, xsd]
    mrsVar <- var
    triple mrsVar (rdf .:. "type") (mrs .:. "MRS")
    (p1, d1) <- middleTransform pred mrsVar Map.empty prefixes
    p1
    selectVars [mrsVar]

middleTransform :: PredExpr -> QG.Variable -> VariablesMap -> [QG.Prefix] -> Query (Query [QG.Pattern], Query VariablesMap)
middleTransform (P pred) mrsVar dict prefixes =
  atomicTransform pred mrsVar dict prefixes
middleTransform (And pred1 pred2) mrsVar dict prefixes =
  do
    (listT1, d1) <-  middleTransform pred1 mrsVar dict prefixes
    dict1 <- d1
    (listT2, d2) <-  middleTransform pred2 mrsVar dict1 prefixes
    ts1 <- listT1; ts2 <- listT2; dict2 <- d2
    return (return(ts1 ++ ts2), return dict2)
middleTransform (Or pred1 pred2) mrsVar dict prefixes =
  do
    (listT1, d1) <-  middleTransform pred1 mrsVar dict prefixes
    dict1 <- d1
    (listT2, d2) <-  middleTransform pred2 mrsVar dict1 prefixes
    ts1 <- listT1
    ts2 <- listT2
    dict2 <- d2
    orPatterns <- union listT1 listT2
    return (return [orPatterns], return dict2)
-- Need a fix/verification:
middleTransform (Not pred) mrsVar dict prefixes =
  do
    (listT1, d1) <-  middleTransform pred mrsVar dict prefixes
    dict1 <- d1
    ts1 <- listT1
    notPattern <- filterNotExists listT1
    return (return [notPattern], return dict1)
    
atomicTransform :: Predicate -> QG.Variable -> VariablesMap -> [QG.Prefix] -> Query (Query [QG.Pattern], Query VariablesMap)
atomicTransform (Predicate _ (Just var_) Nothing (Just predText) Nothing) mrsVar dict prefixes =
  if Map.member var_ dict
  then atomicTransformK (Predicate False (Just var_) Nothing (Just predText) Nothing) mrsVar dict prefixes
  else atomicTransformN (Predicate False (Just var_) Nothing (Just predText) Nothing) mrsVar dict prefixes
  
atomicTransform (Predicate _ (Just var_) Nothing (Just predText) (Just [])) mrsVar dict prefixes =
  if Map.member var_ dict
  then atomicTransformK (Predicate False (Just var_) Nothing (Just predText) Nothing) mrsVar dict prefixes
  else atomicTransformN (Predicate False (Just var_) Nothing (Just predText) Nothing) mrsVar dict prefixes

atomicTransform (Predicate _ (Just var_) Nothing (Just predText) (Just xs)) mrsVar dict prefixes =
  if Map.member var_ dict
  then atomicTransformK (Predicate False (Just var_) Nothing (Just predText) (Just xs)) mrsVar dict prefixes
  else atomicTransformN (Predicate False (Just var_) Nothing (Just predText) (Just xs)) mrsVar dict prefixes

atomicTransformK :: Predicate -> QG.Variable -> VariablesMap -> [QG.Prefix] -> Query (Query [QG.Pattern], Query VariablesMap)
atomicTransformK (Predicate _ (Just var_) Nothing (Just predText) Nothing) mrsVar dict prefixes =
  do
    let Just ep1Var = Map.lookup var_ dict
    pred1Var <- var    
    t1 <- triple mrsVar (head prefixes .:. "hasEP") ep1Var
    t2 <- triple ep1Var (prefixes!!2 .:. "hasPredicate") pred1Var
    t3 <- triple pred1Var (prefixes!!2 .:. "predText") (iriRef $ T.pack predText) -- não considera regex
    return (return [t1, t2, t3], return dict)

atomicTransformK (Predicate _ (Just var_) Nothing (Just predText) (Just [])) mrsVar dict prefixes =
  do
    let Just ep1Var = Map.lookup var_ dict
    pred1Var <- var    
    t1 <- triple mrsVar (head prefixes .:. "hasEP") ep1Var
    t2 <- triple ep1Var (prefixes!!2 .:. "hasPredicate") pred1Var
    t3 <- triple pred1Var (prefixes!!2 .:. "predText") (iriRef $ T.pack predText) -- não considera regex
    return (return [t1, t2, t3], return dict)

atomicTransformK (Predicate _ (Just var_) Nothing (Just predText) argList) mrsVar dict prefixes =
  do
    let Just ep1Var = Map.lookup var_ dict
    pred1Var <- var    
    t1 <- triple mrsVar (head prefixes .:. "hasEP") ep1Var
    t2 <- triple ep1Var (prefixes!!2 .:. "hasPredicate") pred1Var
    t3 <- triple pred1Var (prefixes!!2 .:. "predText") (iriRef $ T.pack predText) -- não considera regex
    (listT4, d1) <- processArgs argList ep1Var dict prefixes
    t4 <- listT4
    dict1 <- d1
    return (return (t1 : t2 : t3 : t4), return dict1)

atomicTransformN :: Predicate -> QG.Variable -> VariablesMap -> [QG.Prefix] -> Query (Query [QG.Pattern], Query VariablesMap)
atomicTransformN (Predicate _ (Just var_) Nothing (Just predText) Nothing) mrsVar dict prefixes =
  do
    ep1Var <- var
    pred1Var <- var    
    t1 <- triple mrsVar (head prefixes .:. "hasEP") ep1Var
    t2 <- triple ep1Var (prefixes!!2 .:. "hasPredicate") pred1Var
    t3 <- triple pred1Var (prefixes!!2 .:. "predText") (iriRef $ T.pack predText) -- não considera regex
    return (return [t1, t2, t3], return (Map.insert var_ ep1Var dict))

atomicTransformN (Predicate _ (Just var_) Nothing (Just predText) (Just [])) mrsVar dict prefixes =
  do
    ep1Var <- var
    pred1Var <- var    
    t1 <- triple mrsVar (head prefixes .:. "hasEP") ep1Var
    t2 <- triple ep1Var (prefixes!!2 .:. "hasPredicate") pred1Var
    t3 <- triple pred1Var (prefixes!!2 .:. "predText") (iriRef $ T.pack predText) -- não considera regex
    return (return [t1, t2, t3], return (Map.insert var_ ep1Var dict))

atomicTransformN (Predicate _ (Just var_) Nothing (Just predText) argList) mrsVar dict prefixes =
  do
    ep1Var <- var
    pred1Var <- var    
    t1 <- triple mrsVar (head prefixes .:. "hasEP") ep1Var
    t2 <- triple ep1Var (prefixes!!2 .:. "hasPredicate") pred1Var
    t3 <- triple pred1Var (prefixes!!2 .:. "predText") (iriRef $ T.pack predText) -- não considera regex
    let dict1 = Map.insert var_ ep1Var dict
    (listT4, d2) <- processArgs argList ep1Var dict1 prefixes
    t4 <- listT4
    dict2 <- d2
    return (return (t1 : t2 : t3 : t4), return dict2)

processArgs
        :: Maybe [Arg]
           -> QG.Variable
           -> VariablesMap
           -> [Prefix]
           -> Query (Query [QG.Pattern], Query VariablesMap)
-- --processArgs :: Maybe [Arg] -> QG.Variable -> VariablesMap -> Query [QG.Pattern]
processArgs (Just []) epVar dict prefixes =
  return (return [], return dict)
processArgs (Just [x]) epVar dict prefixes =
  case argvar x of
    Nothing -> processArgN x epVar dict prefixes
    Just y -> case Map.lookup y dict of
      Nothing -> processArgN x epVar dict prefixes
      Just z -> processArgK z (rolepat x) epVar dict prefixes

processArgs (Just (x:xs)) epVar dict prefixes =
  do
    (listT1, d1) <- processArgs (Just [x]) epVar dict prefixes
    t1 <- listT1
    dict1 <- d1
    (listT2, d2) <- processArgs (Just xs) epVar dict1 prefixes
    t2 <- listT2
    dict2 <- d2
    return (return (t1 ++ t2), return dict2)

processArgK
        :: QG.Variable -> Data.Pattern
           -> QG.Variable
           -> VariablesMap
           -> [Prefix]
           -> Query (Query [QG.Pattern], Query VariablesMap)
processArgK holeVar role epVar dict prefixes = do
  t1 <-
    triple epVar (head prefixes .:. T.toLower (T.pack role)) holeVar
  return (return [t1], return dict)

processArgN
        :: Arg
           -> QG.Variable
           -> VariablesMap
           -> [Prefix]
           -> Query (Query [QG.Pattern], Query VariablesMap)
processArgN arg epVar dict prefixes =
  do
    let Just x = argvar arg
    holeVar <- var
    let dict1 = Map.insert x holeVar dict
    t1 <- triple epVar (head prefixes .:. T.toLower (T.pack $ rolepat arg)) holeVar
    return (return [t1], return dict1)

