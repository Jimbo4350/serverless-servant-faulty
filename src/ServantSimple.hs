{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module ServantSimple where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Database
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Database.Persist
import           Servant
import           System.IO



-- * api

type ItemApi = Get '[JSON] [Entity Item]

itemApi :: Proxy ItemApi
itemApi = Proxy

server :: Server ItemApi
server = getItems

getItems :: Handler [Entity Item]
getItems = liftIO bananasQuery
