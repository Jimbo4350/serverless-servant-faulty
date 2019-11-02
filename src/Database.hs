{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database where

import           Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Database.Persist.Postgresql (toSqlKey, withPostgresqlPool, runSqlPersistMPool, ConnectionString, withPostgresqlConn, runMigration, SqlPersistT, printMigration)
import Data.Aeson
import Database.Persist
import Database.Persist.TH
import Database.Persist.Class
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
            [persistLowerCase|
              Item sql=item
                    food String
                    amount Int
                    deriving Show
            |]

instance ToJSON (Entity Item) where
  toJSON = keyValueEntityToJSON

instance FromJSON (Entity Item) where
  parseJSON = keyValueEntityFromJSON

instance FromJSON Item where
  parseJSON = withObject "Item" $ \ v ->
    Item <$> v .:  "food"
         <*> v .:  "amount"


instance ToJSON Item where
    toJSON (Item food amount) =
        object [ "food" .= food
               , "amount" .= amount
               ]

connString :: ConnectionString
connString = "xxxxx"

fullTest :: IO ()
fullTest = runStdoutLoggingT $ withPostgresqlPool connString 10 $ \pool ->
             liftIO $ flip runSqlPersistMPool pool $ do
                printMigration migrateAll
                fan <- insert $ Item "fan" 37
                res  :: [Entity Item] <- selectList [] [LimitTo 1]
                liftIO $ print res
           --     liftIO $ print fan


runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connString action =
  runStdoutLoggingT $ withPostgresqlConn connString $ \backend ->
    runReaderT action backend

selectbananas :: MonadIO m => SqlPersistT m [Entity Item]
selectbananas = selectList [ItemFood ==. "fan"] [LimitTo 1]

bananasQuery :: IO [Entity Item]
bananasQuery = runAction connString selectbananas

--insertBananas :: MonadIO m => SqlPersistT m (Key Item)
--insertBananas = Entity $ (toSqlKey 1 $ Item "ban" 4)

--insertBananasTest :: IO (Key Item)
--insertBananasTest = runAction connString insertBananas
-- bananasInsert = runAction connectIn
-- selectYoungTeachers' :: (MonadIO m) => SqlPersistT m [Entity User]
-- selectYoungTeachers' = selectList
--   [UserAge <. 25, UserOccupation ==. "Teacher"] [Asc UserEmail, OffsetBy 5, LimitTo 100]
