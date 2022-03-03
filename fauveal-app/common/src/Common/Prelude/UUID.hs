{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Common.Prelude.UUID
  ( UUID (..),
    newRandomUUID,
    generateBase32UID,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
  ( FromJSON,
    FromJSONKey,
    ToJSON,
    ToJSONKey,
  )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Random (Random (randomRs))
import System.Random.TF.Init (initTFGen)

newtype UUID (a :: * -> *) = UUID Text
  deriving stock (Eq, Ord, Show, Generic, Read)
  deriving newtype (NFData)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newRandomUUID :: MonadIO m => m (UUID a)
newRandomUUID = liftIO $ generateBase32UID (\range -> randomRs range <$> initTFGen)

base32digits :: Map Int Char
base32digits = Map.fromList $ zip [0 ..] "0123456789abcdefghjkmnpqrstvwxyz"

-- | Given the ability to generate a stream of integers in a given range,
-- generate a random 'UUID'.
generateBase32UID :: Functor m => ((Int, Int) -> m [Int]) -> m (UUID a)
generateBase32UID random_rs =
  UUID . T.pack . fmap (\k -> Map.findWithDefault '0' k base32digits) . take base32UIDLength
    <$> random_rs (0, 31)

base32UIDLength :: Int
base32UIDLength = 22
