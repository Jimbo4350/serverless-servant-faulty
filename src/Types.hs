{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where

import    Data.Aeson
import    Data.ByteString
import    GHC.Generics
import    Network.HTTP.Types

-- Input
data Event = Event
  { resource :: String
  --, httpMethod :: Method
  --, queryStringParameters :: [(ByteString, Maybe ByteString)]
  --, headers :: RequestHeaders
  , body :: String -- Incoming JSON is here.
  }

instance FromJSON Event where
  parseJSON = withObject "Event" $ \v -> Event
      <$> v .: "resource"
      <*> v .: "body"

-- Output
data Response = Response
  { statusCode:: Int
  , body :: String
  } deriving (Generic, ToJSON, FromJSON)
