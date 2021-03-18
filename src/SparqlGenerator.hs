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

data TransformData = TransformData
  { varDict :: Query VariablesMap
  , mrsVar :: QG.Variable
  , prefixes :: [QG.Prefix]
  , patterns :: Query [QG.Pattern] }

transformation :: PredExpr -> Query SelectQuery
transformation pred =
  do
    mrs <- mrs ; erg <- erg; delph <- delph; rdf <- rdf; rdfs <- rdfs; xsd <- xsd

    let prefixes = [mrs, erg, delph, rdf, rdfs, xsd]
    mrsVar <- var
    triple mrsVar (rdf .:. "type") (mrs .:. "MRS")
    s <- middleTransform pred $ return $ TransformData (return Map.empty) mrsVar prefixes (return [])
    patterns s
    selectVars [mrsVar]

middleTransform :: PredExpr -> Query TransformData -> Query TransformData
middleTransform (P pred) s =
  atomicTransform pred s
middleTransform (And pred1 pred2) s =
  do
    os <- s
    p0 <- patterns os
    s1 <- middleTransform pred1 $ return $ TransformData (varDict os) (mrsVar os) (prefixes os) (return [])
    s2 <- middleTransform pred2 $ return $ TransformData (varDict s1) (mrsVar os) (prefixes os) (return [])
    p1 <- patterns s1
    p2 <- patterns s2
    return $ TransformData (varDict s2) (mrsVar os) (prefixes os) (return $ p0 ++ p1 ++ p2)
    
middleTransform (Or pred1 pred2) s =
  do
    os <- s
    p0 <- patterns os
    s1 <- middleTransform pred1 $ return $ TransformData (varDict os) (mrsVar os) (prefixes os) (return [])
    s2 <- middleTransform pred2 $ return $ TransformData (varDict s1) (mrsVar os) (prefixes os) (return [])
    let p1 = patterns s1
        p2 = patterns s2
    t1 <- p1 `union` p2 -- p1 :: Pattern
    return $ TransformData (varDict s2) (mrsVar os) (prefixes os) (return $ p0 ++ [t1])
    
-- Need a fix/verification:
middleTransform (Not pred) s =
  do
    os <- s
    p0 <- patterns os
    s1 <- middleTransform pred $ return $ TransformData (varDict os) (mrsVar os) (prefixes os) (return [])
    p1 <- patterns s1
    t1 <- filterNotExists $ return $ p0 ++ p1
    return $ TransformData (varDict s1) (mrsVar os) (prefixes os) (return [t1])


atomicTransform :: Predicate -> Query TransformData -> Query TransformData
atomicTransform pred@(Predicate top Nothing predMod predPred argList) s =
  do
    s1 <- s
    epVar <- var
    s2 <- addingTriple
          (triple (mrsVar s1) (head (prefixes s1) .:. "hasEP") epVar)
          (return s1)
    s3 <- putTop pred epVar (return s2)
    --Falta processar predicado direito; considerando predmod e regex.
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
    --Falta processar predicado direito; considerando predmod e regex.
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
        s2 <- addingTriple
              (triple v (prefixes os!!2 .:. "predText") (T.pack predText))
              (return s1)
        return s2

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
  do
    v <- var
    os <- s
    s1 <- addingTriple
          (triple epVar (head (prefixes os) .:. T.pack role) v)
          s
    return s1
processArg (Arg role (Just holeName)) epVar s =
  do
    s1 <- createVar holeName s
    dict <- varDict s1
    let Just v = Map.lookup holeName dict
    s2 <- addingTriple
          (triple epVar (head (prefixes s1) .:. T.pack role) v)
          (return s1)
    return s2

createVar :: Data.Variable -> Query TransformData -> Query TransformData
createVar varName s =
  do
    os <- s
    dict <- varDict os
    if Map.member varName dict
    then
      s
    else
      do
        v <- var;
        return $ TransformData (return $ Map.insert varName v dict) (mrsVar os) (prefixes os) (patterns os)

    -- v <- var
    -- return $ TransformData (return $ Map.insert varName v dict) (mrsVar os) (prefixes os) (patterns os)

addingTriple :: Query QG.Pattern -> Query TransformData -> Query TransformData
addingTriple t s =
  do
    pat <- t
    os <- s
    pat0 <- patterns os
    return $ TransformData (varDict os) (mrsVar os) (prefixes os) (return $ pat0 ++ [pat]) 
