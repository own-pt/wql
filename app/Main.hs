{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson         hiding (json)
import           Data.Monoid        ((<>))
import           Data.Text          (Text, pack, unpack)
import           GHC.Generics

import           SparqlGenerator    (generateOptSPARQL)
import Database.HSparql.Connection  (selectQueryRaw)
import Text.ParserCombinators.ReadP hiding (get)

data Person = Person
  { name :: Text
  , age  :: Int
  } deriving (Generic, Show)

newtype EntWql = EntWql
  { q :: Text
  } deriving (Generic, Show)

instance ToJSON Person

instance FromJSON Person

instance ToJSON EntWql

instance FromJSON EntWql

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: Api
app = do
  get "people" $ do
    json $ Person { name = "Fry", age = 25 }
  post "people" $ do
    thePerson <- jsonBody' :: ApiAction Person
    text $ "Parsed: " <> pack (show thePerson)
  post "query" $ do
    thePerson <- jsonBody' :: ApiAction EntWql
    -- let x = selectQueryRaw "http://TURASTATION:10035/repositories/gold-erg" (generateOptSPARQL "[ARG3 x]")
    text $ pack (ppSPARQL $ unpack $ q thePerson)

-- Code to make pretty printing of the SPARQL
data SPARQLObject = SPARQLObject
  { prefixes :: [String]
  , selectLine :: String
  , lines :: [(String, Int)]
  } 

instance Show SPARQLObject where
  show (SPARQLObject prefs sl ls) =
    let
    ss = unlines (prefs ++ [sl])
    ws = concatMap (\(st, i) -> replicate i ' ' ++ st ++ "\n") ls
    in ss ++ ws

getPrefix :: ReadP String
getPrefix = do
  pr <- string "PREFIX "
  name <- munch (/= '<')
  rest <- munch (/= ' ')
  skipSpaces
  return $ pr ++ name ++ rest

getPrefixes :: ReadP [String]
getPrefixes = many1 getPrefix

parseSelect :: ReadP String
parseSelect = do
  sl <- munch1 (/= '{')
  c <- char '{'
  skipSpaces
  return $ sl ++ [c]

parseLineAux0 :: ReadP String
parseLineAux0 =
  string "FILTER NOT EXISTS {" <* skipSpaces

parseLineAux1 =
  string "} UNION {" <* skipSpaces

parseLineAux2 = 
  string "{" <* skipSpaces

parseLineAux3 = do
  string "}" <* skipSpaces

parseLineAux4 = do
  s1 <- string "FILTER (REGEX("
  s2 <- munch1 (`notElem` (")" :: String))
  s3 <- munch1 (`notElem` (".}{" :: String))
  p <- char '.'
  skipSpaces
  return $ s1 ++ s2 ++ s3 ++ [p]

parseLineAux5 = do
  sl <- munch1 (`notElem` (".}{" :: String))
  p <- char '.'
  skipSpaces
  return $ sl ++ [p]

parseLinesAux :: ReadP [String]
parseLinesAux = many1 $ parseLineAux0 <++ parseLineAux1 <++ parseLineAux2 <++ parseLineAux3 <++ parseLineAux4 <++ parseLineAux5

getIndentation :: ReadP [(String, Int)]
getIndentation = do
  ls <- parseLinesAux
  let ls2 = createInts $ zip ls $ tail ls
  return $ zip ls ls2
  where
    createInts x = foldl f [2] x
    f inds (s1, s2) =
      case Prelude.head s2 of
        '}' -> inds ++ [last inds - 2]
        _ -> case last s1 of
          '{' -> inds ++ [last inds + 2]
          '}' -> inds ++ [last inds - 2]
          _   -> inds ++ [last inds]

createObj = do
  prefs <- getPrefixes
  sl <- parseSelect
  ls <- getIndentation
  return $ SPARQLObject prefs sl ls

ppSPARQL = show . fst . last . readP_to_S  createObj . generateOptSPARQL
