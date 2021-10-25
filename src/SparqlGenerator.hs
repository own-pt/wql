{-# LANGUAGE OverloadedStrings #-}

module SparqlGenerator where

import GHC.Unicode ( isSpace, isAlpha, isDigit, isAlphaNum )
import Text.ParserCombinators.ReadP as RP
import Data 
import Database.HSparql.QueryGenerator as QG
import Database.HSparql.Connection
import qualified Data.Text as T
import qualified Data.Map as Map
import WQL (wql, pushNots)
-- import Debug.Trace
       
mrs = prefix "mrs" (iriRef "http://www.delph-in.net/schema/mrs#")
--erg = prefix "erg" (iriRef "http://www.delph-in.net/schema/erg#")
delph = prefix "delph" (iriRef "http://www.delph-in.net/schema/")
rdf = prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
rdfs = prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")
--xsd = prefix "xsd" (iriRef "http://www.w3.org/2001/XMLSchema#")

type VariablesMap = Map.Map Data.Variable QG.Variable 

data TransformData = TransformData
  { varDict :: Query VariablesMap
  , mrsVar :: QG.Variable
  , prefixes :: [QG.Prefix]
  , patterns :: Query [QG.Pattern]
  , selectList :: [QG.Variable]
  }

generateSPARQL = createSelectQuery . wqlTransformation . fst . last . readP_to_S wql 

generateOptSPARQL = createSelectQuery . wqlTransformation . pushNots . fst . last . readP_to_S wql 

wqlTransformation w@(WQL p h) =
  do
    mrs <- mrs ; delph <- delph; rdf <- rdf; rdfs <- rdfs
    let prefixes = [mrs, delph, rdf, rdfs]
    mrsVar <- var
    s0 <- pure $
          TransformData
          (return Map.empty)
          mrsVar
          prefixes
          (pure [])
          [mrsVar]
    s1 <- addingTriple (triple mrsVar (rdf .:. "type") (mrs .:. "MRS")) (pure s0)
    s2 <- predExprTransformation p (pure s1)
    s3 <- consTransformation h (pure s2)
    {-
    dd <- varDict s3
    traceM $ show dd
    -}
    patterns s3
    selectVars $ selectList s3
    
consTransformation :: Maybe [Cons] -> Query TransformData -> Query TransformData
consTransformation (Just (x : xs)) s =
  do
    hconsVar <- var
    s1 <- createVar (high x) s
    s2 <- createVar (low x) (return s1)
    dict <- varDict s2
    let Just highVar = Map.lookup (high x) dict
        Just lowVar = Map.lookup (low x) dict
    s3 <- addingTriple
          (triple
            (mrsVar s2)
            (prefixes s2 !! 0 .:. "hasHcons")
            hconsVar)
          (pure s2)
    s4 <- addingTriple
          (triple
            hconsVar
            (prefixes s2 !! 2 .:. "type")
            (prefixes s2 !! 0 .:. "Qeq"))
          (pure s3)
    s5 <- addingTriple
          (triple
            hconsVar
            (prefixes s2 !! 0 .:. "highHcons")
            highVar)
          (pure s4)
    s6 <- addingTriple
          (triple
            hconsVar
            (prefixes s2 !! 0 .:. "lowHcons")
            lowVar)
          (pure s5)
    consTransformation (Just xs) (pure s6)
consTransformation _ s = s
  
predExprTransformation :: PredExpr -> Query TransformData -> Query TransformData
predExprTransformation (P pred) s =
  atomicTransform pred s
predExprTransformation (And pred1 pred2) s =
  do
    os <- s
    s1 <- predExprTransformation pred1 $ return $ os {patterns = pure []}
    s2 <- predExprTransformation pred2 $ return $ s1 {patterns = pure []}
    pure s2 {patterns =
             patterns os >>= \p0 ->
             patterns s1 >>= \p1 ->
             patterns s2 >>= \p2 ->
               pure $ p0 ++ p1 ++ p2} 
predExprTransformation (Or pred1 pred2) s =
  do
    os <- s
    s1 <- predExprTransformation pred1 $ pure os {patterns = pure []}
    s2 <- predExprTransformation pred2 $ pure s1 {patterns = pure []}
    pure s2 {patterns =
             patterns os >>= \p0 ->
             union (patterns s1) (patterns s2) >>= \p1 ->
               pure $ p0 ++ [p1]}
predExprTransformation (Not pred) s =
  do
    os <- s
    s1 <- predExprTransformation pred $ pure os {patterns = pure []}
    pure s1 {patterns =
             patterns os >>= \p0 ->
             filterNotExists (patterns s1) >>= \p1 ->
                pure $ p0 ++ [p1]}
    
atomicTransform :: Predicate -> Query TransformData -> Query TransformData
atomicTransform pred@(Predicate _ (Just handleName) _ _ _) s =
  do
    s1 <- createVar handleName s
    dict <- varDict s1
    epLabelVar <- var
    epVar <- var
    let Just handleVar = Map.lookup handleName dict
    s2 <- addingTriple
         (triple
           epVar
           (prefixes s1 !! 0 .:. "hasLabel")
           handleVar)
         (pure s1)
    s3 <- addingTriple
         (triple
           epVar
           (prefixes s1 !! 3 .:. "label")
           epLabelVar)
         (pure s2)        
    s4 <- addingTriple
         (triple
           (mrsVar s1)
           (prefixes s1 !! 0 .:. "hasEP")
           epVar)
         (pure s3)
    s5 <- putPred pred epVar (pure s4)
    s6 <- processArgs (predargs pred) epVar (pure s5)
    s7 <- putTop pred handleVar (pure s6)
    pure s7 {selectList = epLabelVar : selectList s7}
    
atomicTransform pred s =
  do
    os <- s
    epVar <- var
    epLabelVar <- var
    s1 <- addingTriple
          (triple
            (mrsVar os)
            (prefixes os !! 0 .:. "hasEP")
            epVar)
          s
    s2 <- addingTriple
         (triple
           epVar
           (prefixes os !! 3 .:. "label")
           epLabelVar)
         (pure s1)
    s3 <- putPred pred epVar (pure s2)
    s4 <- processArgs (predargs pred) epVar (pure s3)
    s5 <- putTop pred epVar (pure s4)
    pure s5 {selectList = epLabelVar : selectList s5}
  
-- hardcoding the creating of the hcons, review later
putTop :: Predicate -> QG.Variable -> Query TransformData -> Query TransformData
putTop predicate handleVar s =
  do
    os <- s
    if predtop predicate
    then
      do
        topH <- var
        hconsVar <- var
        s1 <- addingTriple
              (triple
               (mrsVar os)
               (prefixes os!!1 .:. "hasTop")
               topH)
              s
        s2 <- addingTriple
             (triple
               (mrsVar os)
               (head(prefixes os) .:. "hasHcons")
               hconsVar)
             (pure s1)
        s3 <- addingTriple
             (triple
               hconsVar
               (prefixes os!!2 .:. "type")
               (head (prefixes os) .:. "Qeq"))
             (pure s2)
        s4 <- addingTriple
             (triple
               hconsVar
               (head(prefixes os) .:. "highHcons")
               topH)
             (pure s3)
        s5 <- addingTriple
             (triple
               hconsVar
               (head(prefixes os) .:. "lowHcons")
               handleVar)
              (pure s4)
        pure s5
    else
      s

putPred :: Predicate -> QG.Variable -> Query TransformData -> Query TransformData
putPred (Predicate _ _ modf (Just predText) _) epVar s = 
  do
    os <- s
    predicateVar <- var
    s1 <- addingTriple
          (triple
            epVar
            (prefixes os!!1 .:. "hasPredicate")
            predicateVar)
          s
    s2 <- putPredText predicateVar predText modf (pure s1)
    pure s2
putPred _ epVar s = s

putPredText :: QG.Variable -> Data.Pattern -> Maybe Char -> Query TransformData -> Query TransformData
putPredText predicateVar predText modf s =
  do
    if '*' `elem` predText
      then
        do
          let newPredText = T.replace "*" ".*" $ T.pack $ "^" ++ predText ++ "$"
          os <- s
          predTextVar <- var
          s1 <- addingTriple
                (triple
                  predicateVar
                  (prefixes os!!1 .:. modfToRDFRel modf)
                  predTextVar)
                s
          addingTriple
            (filterExpr $ regex predTextVar newPredText)
            (return s1)
      else
        do
          os <- s
          addingTriple
            (triple
              predicateVar
              (prefixes os!!1 .:. modfToRDFRel modf)
              (T.pack predText))
            s
      where
        modfToRDFRel Nothing = "predText"
        modfToRDFRel (Just '+') = "hasLemma"
        modfToRDFRel (Just '/') = "hasPos"
        modfToRDFRel (Just '=') = "hasSense"
       
processArgs :: Maybe [Arg] -> QG.Variable -> Query TransformData -> Query TransformData
processArgs (Just ((Arg role (Just holeName)):xs)) epVar s =
  do
    os <- createVar holeName s
    dict <- varDict os
    let Just holeNameVar = Map.lookup holeName dict
    case role of
      "*" ->
        do
          s1 <- addingTriple
                (triple epVar (head (prefixes os) .:. T.pack "role") holeNameVar)
                (pure os)
          s2 <- processArgs (Just xs) epVar (pure s1)
          pure s2 {selectList = holeNameVar : selectList s2}
      _ -> if '*' `elem` role
           then
             do
               let newRoleText = T.replace "*" ".*" $ (T.toLower . T.pack) role
               roleV <- var
               s1 <- addingTriple
                     (triple epVar roleV holeNameVar)
                     s
               s2 <- addingTriple
                     (filterExpr $ regex roleV newRoleText)
                     (pure s1)
               s3 <- processArgs (Just xs) epVar (pure s2)
               pure s3 {selectList = holeNameVar : selectList s3}
           else
             do
               s1 <- addingTriple
                     (triple
                      epVar
                      (head (prefixes os) .:. (T.toLower . T.pack) role)
                      holeNameVar)
                     (pure os)
               s2 <- processArgs (Just xs) epVar (pure s1)
               pure s2 {selectList = holeNameVar : selectList s2}
processArgs _ _ s = s

        
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
        pure os {varDict = return $ Map.insert varName v dict}

addingTriple :: Query QG.Pattern -> Query TransformData -> Query TransformData
addingTriple t s =
  do
    os <- s
    pure os {patterns =
              patterns os >>= \op ->
              t >>= \ot ->
                pure $ op ++ [ot]}
    
