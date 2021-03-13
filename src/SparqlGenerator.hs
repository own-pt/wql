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
import Control.Monad.Trans.State.Lazy

-- type VariablesMap = Map.Map Data.Variable (Control.Monad.Trans.State.Lazy.StateT
--        Database.HSparql.QueryGenerator.QueryData
--        Data.Functor.Identity.Identity
--        QG.Variable)

-- Making the Variables map:

--myLookup :: Data.Variable -> VariablesMap -> Query QG.Variable
myLookup search dict =
  case Map.lookup search dict of
    Nothing -> do var
    Just x -> x


--makeVars :: PredExpr -> VariablesMap
makeVars pred = makeVarsAux pred Map.empty

--makeVarsAux :: PredExpr -> VariablesMap -> VariablesMap
makeVarsAux (And pred1 pred2) dict =
  makeVarsAux pred2 newDict
  where
    newDict = makeVarsAux pred1 dict
makeVarsAux (Or pred1 pred2) dict =
  makeVarsAux pred2 newDict
  where
    newDict = makeVarsAux pred1 dict
makeVarsAux (Not pred) dict =
  makeVarsAux pred dict
makeVarsAux (P pred) dict =
  makeVarsPred pred dict

-- makeVarsPred :: Predicate -> VariablesMap -> VariablesMap
makeVarsPred (Predicate _ (Just vari) _ _ argList) dict =
  makeVarsArgs argList newDict
  where newDict = Map.insert vari (do QG.var) dict
makeVarsPred (Predicate _ Nothing _ _ argList) dict =
  makeVarsArgs argList dict
  

-- makeVarsArgs :: Maybe [Arg] -> VariablesMap -> VariablesMap
makeVarsArgs Nothing dict = dict
makeVarsArgs (Just []) dict = dict
makeVarsArgs (Just (x:xs)) dict =
  makeVarsArgs (Just xs) newDict
  where newDict = makeVarsArg x dict

  
--makeVarsArg :: Arg -> VariablesMap -> VariablesMap
makeVarsArg (Arg _ Nothing) dict =
  dict
makeVarsArg (Arg _ (Just x)) dict =
  Map.insert x y dict
  where y = do var

  
-- now the SPARQL generator        
mrs = prefix "mrs" (iriRef "http://www.delph-in.net/schema/mrs#")
erg = prefix "erg" (iriRef "http://www.delph-in.net/schema/erg#")
delph = prefix "delph" (iriRef "http://www.delph-in.net/schema/")
rdf = prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
rdfs = prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")
xsd = prefix "xsd" (iriRef "http://www.w3.org/2001/XMLSchema#")


transformation :: PredExpr -> Query SelectQuery
transformation pred =
  do
    mrs <- mrs ; erg <- erg; delph <- delph; rdf <- rdf; rdfs <- rdfs; xsd <- xsd
    --erg <- prefix "erg" (iriRef "http://www.delph-in.net/schema/erg#")
    -- delph <- prefix "delph" (iriRef "http://www.delph-in.net/schema/")
    -- rdf <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    -- rdfs <- prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")
    -- xsd <- prefix "xsd" (iriRef "http://www.w3.org/2001/XMLSchema#")

    let prefixes = [mrs, erg, delph, rdf, rdfs, xsd]
    mrsVar <- var
    triple mrsVar (rdf .:. "type") (mrs .:. "MRS")
    middleTransform pred mrsVar dict prefixes

    selectVars [mrsVar]
  where dict = makeVars pred


--middleTransform :: PredExpr -> QG.Variable -> VariablesMap -> Query [QG.Pattern]
middleTransform (P pred) mrsVar dict prefixes =
  atomicTransform pred mrsVar dict prefixes
middleTransform (And pred1 pred2) mrsVar dict prefixes =
  do
    listT1 <- middleTransform pred1 mrsVar dict prefixes
    listT2 <- middleTransform pred2 mrsVar dict prefixes
    return (listT1 ++ listT2)
middleTransform (Or pred1 pred2) mrsVar dict prefixes =
  do
    orPatterns <- union
                  (middleTransform pred1 mrsVar dict prefixes)
                  (middleTransform pred2 mrsVar dict prefixes)
    return [orPatterns]
    
-- Need a fix/verification:
middleTransform (Not pred) mrsVar dict prefixes =
  do
    notPattern <- filterNotExists $ middleTransform pred mrsVar dict prefixes
    return [notPattern]

  
-- atomicTransform ::
--   Predicate  -> QG.Variable -> VariablesMap -> Query [QG.Pattern]
atomicTransform (Predicate _ (Just var_) Nothing (Just predText) Nothing) mrsVar dict prefixes =
  do
    ep1Var <- myLookup var_ dict
    pred1Var <- var
    
    t1 <- triple mrsVar (prefixes!!0 .:. "hasEP") ep1Var
    t2 <- triple ep1Var (prefixes!!2 .:. "hasPredicate") pred1Var
    t3 <- triple pred1Var (prefixes!!2 .:. "predText") (iriRef $ T.pack predText) -- não considera regex
    --listArgsTuples <- processArgs argList ep1Var dict
    return [t1, t2, t3]
atomicTransform (Predicate _ (Just var_) Nothing (Just predText) (Just [])) mrsVar dict prefixes =
  do
    ep1Var <- myLookup var_ dict
    pred1Var <- var
    
    t1 <- triple mrsVar (prefixes!!0 .:. "hasEP") ep1Var
    t2 <- triple ep1Var (prefixes!!2 .:. "hasPredicate") pred1Var
    t3 <- triple pred1Var (prefixes!!2 .:. "predText") (iriRef $ T.pack predText) -- não considera regex
    return [t1, t2, t3]
atomicTransform (Predicate _ (Just var_) Nothing (Just predText) argList) mrsVar dict prefixes =
  do
    ep1Var <- myLookup var_ dict
    pred1Var <- var
    
    t1 <- triple mrsVar (prefixes!!0 .:. "hasEP") ep1Var
    t2 <- triple ep1Var (prefixes!!2 .:. "hasPredicate") pred1Var
    t3 <- triple pred1Var (prefixes!!2 .:. "predText") (iriRef $ T.pack predText) -- não considera regex
    t4 <- processArgs argList ep1Var dict prefixes
    return (t1 : t2 : t3 : t4)  

--processArgs :: Maybe [Arg] -> QG.Variable -> VariablesMap -> Query [QG.Pattern]
processArgs (Just [x]) epVar dict prefixes =
  do
    holeVar <- (case argvar x of
                  Just y -> myLookup y dict
                  Nothing -> var)
    t1 <- triple epVar (prefixes!!0 .:. T.pack (rolepat x)) holeVar
    return [t1]
processArgs (Just (x:xs)) epVar dict prefixes =
  do
    holeVar <- (case argvar x of
                  Just y -> myLookup y dict
                  Nothing -> var)
    t1 <- triple epVar (prefixes!!0 .:. T.pack (rolepat x)) holeVar
    t2 <- processArgs (Just xs) epVar dict prefixes
    return (t1 : t2)   


-- testeVarPat :: QG.Variable -> QG.Variable -> QG.Variable -> T.Text -> QG.Variable -> Query [QG.Pattern]
testeVarPat v y z role label =
  do
    rdf <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    rdfs <- prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")
    mrs <- prefix "mrs" (iriRef "http://www.delph-in.net/schema/mrs#")
    
    t1 <- triple v z y
    t2 <- triple z (rdf .:. "type") (mrs .:. "Role")
    t3 <- triple z (rdfs .:. "label") label
    t4 <- filterExpr $ regex label $ T.replace "*" ".*" role
    return [t1, t2, t3, t4]


-- testeVarPat2 :: QG.Variable -> QG.Variable -> QG.Variable -> T.Text -> Query [QG.Pattern]
testeVarPat2 v y z role =
  do
    rdf <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    rdfs <- prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")
    mrs <- prefix "mrs" (iriRef "http://www.delph-in.net/schema/mrs#")
    label <- var
    t1 <- triple v z y
    t2 <- triple z (rdf .:. "type") (mrs .:. "Role")
    t3 <- triple z (rdfs .:. "label") label
    t4 <- filterExpr $ regex label $ T.replace "*" ".*" role
    return [t1, t2, t3, t4]


-- Essas duas funções geram coisas diferentes:

-- query de "x:_run*[ARG1] | [A*]"
teste1 :: Query SelectQuery
teste1 = do
  mrs <- prefix "mrs" (iriRef "http://www.delph-in.net/schema/mrs#")
  erg <- prefix "erg" (iriRef "http://www.delph-in.net/schema/erg#")
  delph <- prefix "delph" (iriRef "http://www.delph-in.net/schema/")
  rdf <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  rdfs <- prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")
  xsd <- prefix "xsd" (iriRef "http://www.w3.org/2001/XMLSchema#")
    
  mrsVar <- var
  ep1Var <- var
  hole1Var <- var
  pred1Var <- var
  ep2Var <- var
  role2Var <- var
  hole2Var <- var
  pred2Var <- var
  
  union
    (do
        t1 <- triple mrsVar (mrs .:. "hasEP") ep1Var
        t2 <- triple ep1Var (mrs .:. "arg1") hole1Var
        t3 <- triple ep1Var (delph .:. "hasPredicate") pred1Var
        t4 <- filterExpr $ regex pred1Var $ T.replace ("*" :: T.Text) (".*" :: T.Text) ("_run*" :: T.Text)
        return [t1, t2, t3, t4])
    (do
        t5 <- triple mrsVar (mrs .:. "hasEP") ep2Var
        t6 <- testeVarPat2 ep2Var role2Var hole2Var "A*"
        return (t5 : t6))
  selectVars [mrsVar]


teste2 :: Query SelectQuery
teste2 = do
  mrs <- prefix "mrs" (iriRef "http://www.delph-in.net/schema/mrs#")
  erg <- prefix "erg" (iriRef "http://www.delph-in.net/schema/erg#")
  delph <- prefix "delph" (iriRef "http://www.delph-in.net/schema/")
  rdf <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  rdfs <- prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")
  xsd <- prefix "xsd" (iriRef "http://www.w3.org/2001/XMLSchema#")
    
  mrsVar <- var
  ep1Var <- var
  hole1Var <- var
  pred1Var <- var
  ep2Var <- var
  role2Var <- var
  hole2Var <- var
  pred2Var <- var
  --label <- var
  
  union
    (do
        t1 <- triple mrsVar (mrs .:. "hasEP") ep1Var
        t2 <- triple ep1Var (mrs .:. "arg1") hole1Var
        t3 <- triple ep1Var (delph .:. "hasPredicate") pred1Var
        t4 <- filterExpr $ regex pred1Var $ T.replace ("*" :: T.Text) (".*" :: T.Text) ("_run*" :: T.Text)
        return [t1, t2, t3, t4])
    (do
        t5 <- triple mrsVar (mrs .:. "hasEP") ep2Var
        label <- var
        t6 <- testeVarPat ep2Var role2Var hole2Var "A*" label
        return (t5 : t6))
  selectVars [mrsVar]
