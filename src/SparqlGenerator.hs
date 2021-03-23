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
wqlTransformation (WQL p Nothing) =
  do
    s <- predTransformation p
    patterns s
    selectVars [mrsVar s]
    
wqlTransformation (WQL p (Just xs)) =
  do
    s <- predTransformation p
    s1 <- consTransformation xs $ return s
    patterns s1
    selectVars [mrsVar s]


consTransformation :: [Cons] -> Query TransformData -> Query TransformData
consTransformation [] s = s
consTransformation (x : xs) s =
  do
    v <- var
    s1 <- createVar (high x) s
    s2 <- createVar (low x) (return s1)
    dict <- varDict s2
    let Just highVar = Map.lookup (high x) dict
        Just lowVar = Map.lookup (low x) dict
    s3 <- addingTriple (triple v (prefixes s1 !! 4 .:. "type") $ (head $ prefixes s1) .:. "Qeq") (return s2)
    s4 <- addingTriple (triple v (head (prefixes s1) .:. "highHcons") highVar) (return s3)
    s5 <- addingTriple (triple v (head (prefixes s1) .:. "lowHcons") lowVar) (return s4)
    consTransformation xs (return s5)
    
  
predTransformation :: PredExpr -> Query TransformData
predTransformation pred =
  do
    mrs <- mrs ; erg <- erg; delph <- delph; rdf <- rdf; rdfs <- rdfs; xsd <- xsd

    let prefixes = [mrs, erg, delph, rdf, rdfs, xsd]
    mrsVar <- var
    triple mrsVar (rdf .:. "type") (mrs .:. "MRS")
    middleTransform pred $ return $ TransformData (return Map.empty) mrsVar prefixes (return [])
    
middleTransform :: PredExpr -> Query TransformData -> Query TransformData
middleTransform (P pred) s =
  atomicTransform pred s
middleTransform (And pred1 pred2) s =
  do
    os <- s
    s1 <- middleTransform pred1 $ return $ TransformData (varDict os) (mrsVar os) (prefixes os) (return [])
    s2 <- middleTransform pred2 $ return $ TransformData (varDict s1) (mrsVar os) (prefixes os) (return [])
    let p0 = patterns os
        p1 = patterns s1
        p2 = patterns s2
    return $ TransformData (varDict s2) (mrsVar os) (prefixes os) ((++) <$> p0 <*> p1 *> p2)
    
middleTransform (Or pred1 pred2) s =
  do
    os <- s
    s1 <- middleTransform pred1 $ return $ TransformData (varDict os) (mrsVar os) (prefixes os) (return [])
    s2 <- middleTransform pred2 $ return $ TransformData (varDict s1) (mrsVar os) (prefixes os) (return [])
    let p0 = patterns os
        p1 = patterns s1
        p2 = patterns s2
    return $ TransformData (varDict s2) (mrsVar os) (prefixes os) (f <$> p0 <*> union p1 p2)
    where
      f xs x = xs ++ [x]
    
-- Need a fix/verification:
middleTransform (Not pred) s =
  do
    os <- s
    s1 <- middleTransform pred $ return $ TransformData (varDict os) (mrsVar os) (prefixes os) (return [])
    let p0 = patterns os
        p1 = patterns s1
    t1 <- filterNotExists p1
    return $ TransformData (varDict s1) (mrsVar os) (prefixes os) (f <$> p0 <*> filterNotExists p1)
    where
      f xs x = xs ++ [x]


atomicTransform :: Predicate -> Query TransformData -> Query TransformData
atomicTransform pred@(Predicate top Nothing predMod predPred argList) s =
  do
    s1 <- s
    epVar <- var
    s2 <- addingTriple
          (triple (mrsVar s1) (head (prefixes s1) .:. "hasEP") epVar)
          (return s1)
    s3 <- putTop pred epVar (return s2)
    s4 <- putPred pred epVar (return s3)
    s5 <- processArgs argList epVar (return s4)
    return s5


atomicTransform pred@(Predicate top (Just epName) predMod predPred argList) s =
  do
    s1 <- createVar epName s
    dict <- varDict s1
    let Just epVar = Map.lookup epName dict
    s2 <- addingTriple
          (triple (mrsVar s1) (head (prefixes s1) .:. "hasEP") epVar)
          (return s1)
    s3 <- putTop pred epVar (return s2)
    s4 <- putPred pred epVar (return s3)
    s5 <- processArgs argList epVar (return s4)
    return s5
    
putTop :: Predicate -> QG.Variable -> Query TransformData -> Query TransformData
putTop predicate epVar s =
  do
    os <- s
    if predtop predicate
    then
      addingTriple (triple (mrsVar os) (prefixes os!!2 .:. "hasTop") epVar) s
    else
      s

putPred :: Predicate -> QG.Variable -> Query TransformData -> Query TransformData
putPred (Predicate _ _ Nothing predicate _) epVar s = --No predicate modifiers
  case predicate of
    Nothing -> s
    Just predText ->
      do
        os <- s
        v <- var
        s1 <- addingTriple
              (triple epVar (prefixes os!!2 .:. "hasPredicate") v)
              s
        s2 <- putPredText v predText (return s1)
        return s2
putPred (Predicate _ _ (Just modf) predicate _) epVar s =
  case predicate of
    Nothing -> s
    Just predText ->
      do
        os <- s
        v <- var 
        s1 <- addingTriple
              (triple epVar (prefixes os!!2 .:. "hasPredicate") v)
              s
        s2 <- putPredTextMod v predText modf (return s1)
        return s2


putPredText :: QG.Variable -> Data.Pattern -> Query TransformData -> Query TransformData
putPredText predicateVar predText s =
  do
    os <- s
    if '*' `elem` predText
      then
      do
        let newPredText = T.replace "*" ".*" $ T.pack predText
        v <- var
        s1 <- addingTriple
              (triple predicateVar (prefixes os!!2 .:. "predText") v)
              s
        s2 <- addingTriple
              (filterExpr $ regex v  newPredText)
              (return s1)
        return s2
      else
      addingTriple
      (triple predicateVar (prefixes os!!2 .:. "predText") (T.pack predText))
      s

putPredTextMod :: QG.Variable -> Data.Pattern -> Char -> Query TransformData -> Query TransformData
putPredTextMod predicateVar predText modf s =
  do
    os <- s
    if '*' `elem` predText
      then
      do
        let newPredText = T.replace "*" ".*" $ T.pack predText
        v <- var
        s1 <- addingTriple
              (triple predicateVar (prefixes os!!2 .:. f modf) v)
              s
        s2 <- addingTriple
              (filterExpr $ regex v  newPredText)
              (return s1)
        return s2
      else
      addingTriple
      (triple predicateVar (prefixes os!!2 .:. f modf) (T.pack predText))
      s
  where f modf = case modf of
          '+' -> "hasLemma"
          '/' -> "hasPos"
          '=' -> "hasSense"

processArgs :: Maybe [Arg] -> QG.Variable -> Query TransformData -> Query TransformData
processArgs Nothing _ s = s
processArgs (Just []) _ s = s
processArgs (Just [x]) epVar s =
  processArg x epVar s
processArgs (Just (x:xs)) epVar s =
  do
    s1 <- processArg x epVar s --Think of efficiency.
    s2 <- processArgs (Just xs) epVar (return s1)
    return s2

processArg :: Arg -> QG.Variable -> Query TransformData -> Query TransformData
processArg (Arg role Nothing) epVar s =
  if '*' `elem` role
      then
      do
        let newRoleText = T.replace "*" ".*" $ T.pack role
        v <- var
        roleV <- var
        s1 <- addingTriple
              (triple epVar roleV v)
              s
        addingTriple
          (filterExpr $ regex roleV newRoleText)
          (return s1)
      else
      do
        v <- var
        os <- s
        addingTriple
          (triple epVar (head (prefixes os) .:. T.pack role) v)
          s
      
processArg (Arg role (Just holeName)) epVar s =
  do
    s1 <- createVar holeName s
    dict <- varDict s1
    let Just v = Map.lookup holeName dict
    if '*' `elem` role
      then
      do
        let newRoleText = T.replace "*" ".*" $ T.pack role
        roleV <- var
        s1 <- addingTriple
              (triple epVar roleV v)
              s
        addingTriple
          (filterExpr $ regex roleV newRoleText)
          (return s1)
      else
      addingTriple
      (triple epVar (head (prefixes s1) .:. T.pack role) v)
      (return s1)
        
--This function don't create a new variable for one that already exists 
createVar :: Data.Variable -> Query TransformData -> Query TransformData
createVar varName s =
  do
    os <- s
    dict <- varDict os
    if Map.member varName dict
    then s
    else
      do
        v <- var
        return $ TransformData (return $ Map.insert varName v dict) (mrsVar os) (prefixes os) (patterns os)

addingTriple :: Query QG.Pattern -> Query TransformData -> Query TransformData
addingTriple t s =
  do
    os <- s
    return $ TransformData (varDict os) (mrsVar os) (prefixes os) (f <$> t <*> patterns os)
    where f x xs = xs ++ [x]

