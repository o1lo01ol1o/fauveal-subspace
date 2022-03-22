{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Backend.Schema where

import Backend.Transaction (Transaction (..))
-- import Common.Schema
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (..))
import qualified Data.Aeson as Json
import Data.Aeson.GADT.TH (deriveJSONGADT)
import qualified Data.ByteString.Lazy as LBS
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Functor.Identity
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.PostgreSQL.Simple as Pg
import qualified Gargoyle.PostgreSQL.Connect as Gargoyle
import Rhyolite.Backend.App

withDb :: MonadIO m => (Pool Pg.Connection -> IO a) -> m a
withDb f = liftIO $
  Gargoyle.withDb "db" $
    \pool -> do
      -- withResource pool $ \conn ->
      --   runBeamPostgres conn $
      --     autoMigrate migrationBackend checkedPgDb
      f pool

data Notification a where
  Notification :: Notification ()

deriving instance Show (Notification a)

concat
  <$> sequence
    [ deriveJSONGADT ''Notification,
      deriveArgDict ''Notification,
      deriveGShow ''Notification,
      deriveGEq ''Notification,
      deriveGCompare ''Notification
    ]

-- notify :: Notification a -> a -> Transaction mode ()
-- notify n a = void $ do
--   let dontCare = ""
--       cmd = "NOTIFY " <> fromString notifyChannel <> ", ?"
--       notification =
--         DbNotification
--           { _dbNotification_schemaName = SchemaName dontCare,
--             _dbNotification_notificationType = NotificationType_Update,
--             _dbNotification_message = n :=> Identity a
--           }
--   Transaction $
--     ReaderT $ \conn ->
--       Pg.execute conn cmd [T.unpack $ T.decodeUtf8 $ LBS.toStrict $ Json.encode notification]
