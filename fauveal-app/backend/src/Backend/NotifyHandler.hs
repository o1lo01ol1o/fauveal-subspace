{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}

module Backend.NotifyHandler where

import Backend.Schema (Notification)
import Backend.Transaction (Transaction)
import Common.App (View (..), ViewSelector (..))
import Data.Functor.Identity
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup
import Rhyolite.Backend.App
import Rhyolite.DB.NotifyListen
  ( DbNotification (_dbNotification_message),
  )

notifyHandler :: forall a. Monoid a => (forall x. (forall mode. Transaction mode x) -> IO x) -> DbNotification Notification -> ViewSelector a -> IO (View a)
notifyHandler _ msg vs = mempty

-- case _dbNotification_message msg of
-- Notification_AddTask :=> Identity task ->
--   pure $ case getOption $ _viewSelector_tasks vs of
--     Nothing -> mempty
--     Just a ->
--       View
--         { _view_tasks = Option $ Just (a, MMap.singleton (primaryKey task) (First task))
--         }
