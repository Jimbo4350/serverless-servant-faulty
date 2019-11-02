{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import GHC.Generics
import Aws.Lambda
import Data.Aeson
import Database
import qualified Data.ByteString.Lazy.Char8 as ByteString
import ServantSimple
import ServantShim
import Servant.Server (serve)
import Types


handler :: Event -> Context -> IO (Either String Response)
handler event context = do
  fromServant <- makeHandler (serve itemApi server) event
  pure $ Right Response
    { statusCode = 200
    , body = "test: " ++ ByteString.unpack fromServant
    }
