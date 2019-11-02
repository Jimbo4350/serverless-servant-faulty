{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module ServantSimple where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

import           Database.Beam
import           Database.Beam.MySQL
import           Database.MySQL.Base


data Item = Item { name :: String }
            deriving (Generic, ToJSON, FromJSON)

-- * api

type ItemApi = Get '[JSON] [Item]

itemApi :: Proxy ItemApi
itemApi = Proxy

server :: Server ItemApi
server = getItems

getItems :: Handler [Item]
getItems = liftIO allItems

allItems :: IO [Item]
allItems = pure $ [Item "This is the good shit"]
