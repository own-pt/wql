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
     
mrs = prefix "mrs" (iriRef "http://www.delph-in.net/schema/mrs#")
erg = prefix "erg" (iriRef "http://www.delph-in.net/schema/erg#")
delph = prefix "delph" (iriRef "http://www.delph-in.net/schema/")
rdf = prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
rdfs = prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")
xsd = prefix "xsd" (iriRef "http://www.w3.org/2001/XMLSchema#")

type VariablesMap = Map.Map Data.Variable QG.Variable 

data TransformData = TransformData
  { varDict :: Query VariablesMap
  , mrsVar :: QG.Variable
  , prefixes :: [QG.Prefix]
  , patterns :: Query [QG.Pattern]
  }

generateSPARQL = createSelectQuery . wqlTransformation . fst . last . readP_to_S wql 

generateOptSPARQL = createSelectQuery . wqlTransformation . pushNots . fst . last . readP_to_S wql 

wqlTransformation :: WQL -> Query SelectQuery    
wqlTransformation w@(WQL p h) =
  do
    mrs <- mrs ; erg <- erg; delph <- delph; rdf <- rdf; rdfs <- rdfs; xsd <- xsd
    let prefixes = [mrs, erg, delph, rdf, rdfs, xsd]
    mrsVar <- var
    let
      s = predExprTransformation
          p
          (return $ TransformData
            (createVarsWQL w)
            mrsVar
            prefixes
            ((: []) <$> triple mrsVar (rdf .:. "type") (mrs .:. "MRS")))
    s1 <- consTransformation h s
    patterns s1
    selectVars [mrsVar]

consTransformation :: Maybe [Cons] -> Query TransformData -> Query TransformData
consTransformation (Just (x : xs)) s =
  do
    v <- var
    os <- s
    dict <- varDict os
    let Just highVar = Map.lookup (high x) dict
        Just lowVar = Map.lookup (low x) dict
        s1 = addingTriple (triple v (prefixes os !! 3 .:. "type") $ head (prefixes os) .:. "Qeq") s
        s2 = addingTriple (triple (mrsVar os) (head (prefixes os) .:. "hasHcons") v) s1
        s3 = addingTriple (triple v (head (prefixes os) .:. "highHcons") highVar) s2
        s4 = addingTriple (triple v (head (prefixes os) .:. "lowHcons") lowVar) s3
    consTransformation (Just xs) s4
consTransformation _ s = s
  
predExprTransformation :: PredExpr -> Query TransformData -> Query TransformData
predExprTransformation (P pred) s =
  atomicTransform pred s
predExprTransformation (And pred1 pred2) s =
  do
    os <- s
    s1 <- predExprTransformation pred1 $ return $ TransformData (varDict os) (mrsVar os) (prefixes os) (return [])
    s2 <- predExprTransformation pred2 $ return $ TransformData (varDict os) (mrsVar os) (prefixes os) (return [])
    let p0 = patterns os
        p1 = patterns s1
        p2 = patterns s2
    return $ TransformData (varDict s2) (mrsVar os) (prefixes os) ((++) <$> p0 <*> p1 *> p2)
    
predExprTransformation (Or pred1 pred2) s =
  do
    os <- s
    s1 <- predExprTransformation pred1 $ return $ TransformData (varDict os) (mrsVar os) (prefixes os) (return [])
    s2 <- predExprTransformation pred2 $ return $ TransformData (varDict os) (mrsVar os) (prefixes os) (return [])
    let p0 = patterns os
        p1 = patterns s1
        p2 = patterns s2
    return $ TransformData (varDict s2) (mrsVar os) (prefixes os) ((:) <$> union p1 p2 <*> p0)
    
predExprTransformation (Not pred) s =
  do
    os <- s
    s1 <- predExprTransformation pred $ return $ TransformData (varDict os) (mrsVar os) (prefixes os) (return [])
    let p0 = patterns os
        p1 = patterns s1
    return $ TransformData (varDict s1) (mrsVar os) (prefixes os) ((:) <$> filterNotExists p1 <*> p0)


atomicTransform :: Predicate -> Query TransformData -> Query TransformData
atomicTransform pred@(Predicate top Nothing predMod predPred argList) s =
  do
    os <- s
    epVar <- var
    let s1 = addingTriple (triple (mrsVar os) (head (prefixes os) .:. "hasEP") epVar) s
        s2 = putTop pred epVar s1
        s3 = putPred pred epVar s2
        s4 = processArgs argList epVar s3
    s4

atomicTransform pred@(Predicate top (Just epName) predMod predPred argList) s =
  do
    os <- s
    dict <- varDict os
    let Just epVar = Map.lookup epName dict
    let s1 = addingTriple (triple (mrsVar os) (head (prefixes os) .:. "hasEP") epVar) s
        s2 = putTop pred epVar s1
        s3 = putPred pred epVar s2
        s4 = processArgs argList epVar s3
    s4

-- hardcoding the creating of the hcons, review later
putTop :: Predicate -> QG.Variable -> Query TransformData -> Query TransformData
putTop predicate epVar s =
  do
    os <- s
    if predtop predicate
    then
      do
        topH <- var
        hconsVar <- var
        let s1 = addingTriple (triple (mrsVar os) (prefixes os!!2 .:. "hasTop") topH) s
            s2 = addingTriple (triple hconsVar (prefixes os!!3 .:. "type") (head (prefixes os) .:. "Qeq")) s1
            s3 = addingTriple (triple (mrsVar os) (head(prefixes os) .:. "hasHcons") hconsVar) s2
            s4 = addingTriple (triple hconsVar (head(prefixes os) .:. "highHcons") topH) s3
            s5 = addingTriple (triple hconsVar (head(prefixes os) .:. "lowHcons") epVar) s4
        s5
    else
      s

putPred :: Predicate -> QG.Variable -> Query TransformData -> Query TransformData
putPred (Predicate _ _ modf (Just predText) _) epVar s = 
  do
    os <- s
    v <- var
    let s1 = addingTriple (triple epVar (prefixes os!!2 .:. "hasPredicate") v) s
        s2 = putPredText v predText modf s1
    s2
putPred _ epVar s = s

putPredText :: QG.Variable -> Data.Pattern -> Maybe Char -> Query TransformData -> Query TransformData
putPredText predicateVar predText modf s =
  do
    os <- s
    if '*' `elem` predText
      then
      do
        v <- var
        let newPredText = T.replace "*" ".*" $ T.pack predText
            s1 = case modf of
                   Nothing -> addingTriple (triple predicateVar (prefixes os!!2 .:. "predText") v) s
                   Just '+' -> addingTriple (triple predicateVar (prefixes os!!2 .:. "hasLemma") v) s
                   Just '/' -> addingTriple (triple predicateVar (prefixes os!!2 .:. "hasPos") v) s
                   Just '=' -> addingTriple (triple predicateVar (prefixes os!!2 .:. "hasSense") v) s
            s2 = addingTriple (filterExpr $ regex v  newPredText) s1
        s2
      else
      addingTriple (triple predicateVar (prefixes os!!2 .:. "predText") (T.pack predText)) s

processArgs :: Maybe [Arg] -> QG.Variable -> Query TransformData -> Query TransformData
processArgs (Just ((Arg role (Just holeName)):xs)) epVar s =
  do
    os <- s
    dict <- varDict os
    let Just v = Map.lookup holeName dict
    let s1 = case role of
               "*" -> addingTriple (triple epVar (head (prefixes os) .:. T.pack "role") v) s
               _ -> if '*' `elem` role
                    then
                      do
                        roleV <- var
                        let newRoleText = T.replace "*" ".*" $ (T.toLower . T.pack) role
                            s11 = addingTriple (triple epVar roleV v) s
                            s12 = addingTriple (filterExpr $ regex roleV newRoleText) s11
                        s12
                    else
                      addingTriple (triple epVar (head (prefixes os) .:. (T.toLower . T.pack) role) v) s
        s2 = processArgs (Just xs) epVar s1
    s2
processArgs (Just ((Arg role Nothing):xs)) epVar s =
  do
    os <- s
    dict <- varDict os
    v <- var
    let s1 = case role of
               "*" -> addingTriple (triple epVar (head (prefixes os) .:. T.pack "role") v) s
               _ -> if '*' `elem` role
                    then
                      do
                        roleV <- var
                        let newRoleText = T.replace "*" ".*" $ (T.toLower . T.pack) role
                            s11 = addingTriple (triple epVar roleV v) s
                            s12 = addingTriple (filterExpr $ regex roleV newRoleText) s11
                        s12
                    else
                      addingTriple (triple epVar (head (prefixes os) .:. (T.toLower . T.pack) role) v) s
        s2 = processArgs (Just xs) epVar s1
    s2
processArgs _ _ s = s
        
addingTriple :: Query QG.Pattern -> Query TransformData -> Query TransformData
addingTriple t s =
  do
    os <- s
    return $ TransformData (varDict os) (mrsVar os) (prefixes os) ((:) <$> t <*> patterns os)

-- Creating the map of WQL variables and SPARQL variables beforehand:
createVarsWQL :: WQL -> Query VariablesMap
createVarsWQL (WQL p h) = dict2
  where
    dict1 = _createVarsPredExpr p $ return Map.empty
    dict2 = _createVarsHcons h dict1
  
_createVarsPredExpr :: PredExpr -> Query VariablesMap -> Query VariablesMap
_createVarsPredExpr (P pred) dict = _createVarsPred pred dict
_createVarsPredExpr (Not pred) dict = _createVarsPredExpr pred dict
_createVarsPredExpr (And pred1 pred2) dict = dict2
  where
    dict1 = _createVarsPredExpr pred1 dict
    dict2 = _createVarsPredExpr pred2 dict1
_createVarsPredExpr (Or pred1 pred2) dict = dict2
  where
    dict1 = _createVarsPredExpr pred1 dict
    dict2 = _createVarsPredExpr pred2 dict1
    
_createVarsPred :: Predicate -> Query VariablesMap -> Query VariablesMap
_createVarsPred (Predicate _ predVar _ _ argList) dict = dict2
  where
    dict1 =  _createVar predVar dict
    dict2 =  _createVarsArgs argList dict1

_createVarsArgs :: Maybe [Arg] -> Query VariablesMap -> Query VariablesMap
_createVarsArgs (Just ((Arg _ varName):xs)) dict = dict2
  where
    dict1 = _createVar varName dict
    dict2 = _createVarsArgs (Just xs) dict1
_createVarsArgs _ dict = dict

_createVarsHcons :: Maybe [Cons] -> Query VariablesMap -> Query VariablesMap
_createVarsHcons (Just ((Cons hvar lvar):xs)) dict = dict3
  where
    dict1 = _createVar (Just hvar) dict
    dict2 = _createVar (Just lvar) dict1
    dict3 = _createVarsHcons (Just xs) dict2    
_createVarsHcons _ dict = dict

_createVar :: Maybe Data.Variable -> Query VariablesMap -> Query VariablesMap
_createVar (Just varName) dict =
  do
    d <- dict
    if Map.member varName d
    then dict
    else
      do
        qv <- var
        return $ Map.insert varName qv d
_createVar _ dict = dict
