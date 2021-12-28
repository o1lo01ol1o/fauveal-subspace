{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Backend.ViewSelectorHandler where

import Backend.Transaction (Transaction, runQuery)
import Common.App (View (..), ViewSelector (..))
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup

viewSelectorHandler :: (Eq a, Monoid a) => (forall x. (forall mode. Transaction mode x) -> IO x) -> ViewSelector a -> IO (View a)
viewSelectorHandler runTransaction vs = mempty

-- if vs == mempty
--   then pure mempty
--   else runTransaction $ do
--     tasks <- fmap Option $ case getOption (_viewSelector_tasks vs) of
--       Nothing -> pure Nothing
--       Just a -> do
--         tasks <- runQuery $ runSelectReturningList $ select $ all_ (_dbTask db)
--         pure $ Just (a, MMap.fromList ([(pk t, First t) | t <- tasks]))
--     pure $
--       View
--         { _view_tasks = tasks
--         }
