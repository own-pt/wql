{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson        hiding (json)
import           Data.Monoid       ((<>))
import           Data.Text         (Text, pack, unpack)
import           GHC.Generics

import           SparqlGenerator   (generateOptSPARQL)
import Database.HSparql.Connection (selectQueryRaw)

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
    text $ pack (generateOptSPARQL $ unpack $ q thePerson)
