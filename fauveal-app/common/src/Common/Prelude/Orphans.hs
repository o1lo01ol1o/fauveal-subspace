{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.Prelude.Orphans where

import Control.DeepSeq (NFData (..))
import Data.Aeson
  ( FromJSON (parseJSON),
    FromJSONKey,
    ToJSON (toJSON),
    ToJSONKey,
  )
import Data.Functor.Const (Const)
import Data.Functor.Product (Product (..))
import Data.Semigroup (Option (Option))
import Data.Time (Day (..))
import Data.Witherable (Filterable (mapMaybe), Witherable (wither))
import Database.Id.Class (Id (..), IdData)
import Reflex.Query.Class (SelectedCount (..))
import Rhyolite.Aeson.Orphans ()
import System.Random (Random, StdGen)

deriving newtype instance Random Day

instance ToJSON StdGen where
  toJSON = toJSON . show

instance FromJSON StdGen where
  parseJSON = fmap read . parseJSON

instance (Semigroup (f x), Semigroup (g x)) => Semigroup (Product f g x) where
  (<>) (Pair fa ga) (Pair fb gb) = Pair (fa <> fb) (ga <> gb)

instance (Monoid (f x), Monoid (g x)) => Monoid (Product f g x) where
  mappend = (<>)
  mempty = Pair mempty mempty

instance (ToJSON a, ToJSONKey a) => ToJSONKey (Const a b)

instance (FromJSON a, FromJSONKey a) => FromJSONKey (Const a b)

-- IdData is a type family
instance NFData (IdData a) => NFData (Id a) where
  rnf (Id x) = rnf x

-------------------------------------------------------------------------------
-- reflex
-------------------------------------------------------------------------------

deriving newtype instance NFData SelectedCount

deriving anyclass instance (ToJSONKey a, ToJSONKey b, ToJSON a, ToJSON b) => ToJSONKey (Either a b)

deriving anyclass instance (FromJSONKey a, FromJSONKey b, FromJSON a, FromJSON b) => FromJSONKey (Either a b)
