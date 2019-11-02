{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import GHC.Generics
import Aws.Lambda
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import ServantSimple
import ServantShim
import Servant.Server (serve)
import Types



greet :: Item -> String
greet (Item val) = val

handler :: Event -> Context -> IO (Either String Response)
handler event context = do
  fromServant <- makeHandler (serve itemApi server) event
  case decode fromServant of
    Just itm ->
      pure $ Right Response
        { statusCode = 200
        , body = greet itm
        }
    Nothing ->
      pure $ Right Response
        { statusCode = 200
        , body = "bad item chief" ++ ByteString.unpack fromServant
        }
