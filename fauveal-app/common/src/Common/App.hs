{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.App where

import Control.Lens (_1)
import Control.Lens.TH (makeLenses)
import Control.Monad.Representable.Reader (Representable)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import qualified Data.Aeson as Json
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Aeson.TH (deriveJSON)
import Data.Align (Align (nil), Semialign (alignWith))
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Distributive (Distributive (collect))
import Data.Distributive.Generic (genericCollect)
import Data.Map.Monoidal (MonoidalMap)
import Data.Monoid (First (..))
import Data.MonoidMap (MonoidMap)
import Data.Semigroup (Option (..))
import Data.Witherable (Filterable (mapMaybe))
import GHC.Generics (Generic, Generic1)
import Reflex.Patch (Additive, Group (negateG))
import Reflex.Query.Class (Query (QueryResult, crop), SelectedCount (..))
import Rhyolite.App (PositivePart (positivePart), standardPositivePart)

data Two a = Two a a
  deriving stock (Eq, Ord, Show, Functor, Traversable, Foldable, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Four a = Four a a a a
  deriving stock (Eq, Ord, Show, Functor, Traversable, Foldable, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving instance Generic1 Two

deriving instance Generic1 Four

instance Distributive Four where
  collect = genericCollect

instance Distributive Two where
  collect = genericCollect

deriving instance Representable Four

deriving instance Representable Two

data PublicRequest a where
  PublicRequest_VAE8 :: Four (Two Float) -> PublicRequest (Maybe [Four Float])

deriving instance Show a => Show (PublicRequest a)

concat
  <$> sequence
    [ deriveJSONGADT ''PublicRequest,
      deriveArgDict ''PublicRequest
    ]

data PrivateRequest a where
  PrivateRequest :: PrivateRequest ()

concat
  <$> sequence
    [ deriveJSONGADT ''PrivateRequest,
      deriveArgDict ''PrivateRequest
    ]

deriving instance Show a => Show (PrivateRequest a)

nullToNothing :: Foldable f => f a -> Maybe (f a)
nullToNothing a = if null a then Nothing else Just a

mapMaybe2Deep :: (Foldable t, Filterable f, Filterable t) => (a -> Maybe b) -> f (t a) -> f (t b)
mapMaybe2Deep f = mapMaybe (nullToNothing . mapMaybe f)

newtype ViewSelector a = ViewSelector
  { _viewSelector_tasks :: Option a
  }
  deriving (Eq, Functor, Generic, Show)

deriveJSON Json.defaultOptions 'ViewSelector

makeLenses 'ViewSelector

instance Semigroup a => Semigroup (ViewSelector a) where
  a <> b =
    ViewSelector
      { _viewSelector_tasks = _viewSelector_tasks a <> _viewSelector_tasks b
      }

instance Semigroup a => Monoid (ViewSelector a) where
  mempty = ViewSelector mempty
  mappend = (<>)

instance Semialign ViewSelector where
  alignWith f a b =
    ViewSelector
      { _viewSelector_tasks = alignWith f (_viewSelector_tasks a) (_viewSelector_tasks b)
      }

instance Align ViewSelector where
  nil = ViewSelector nil

instance (Group a) => Group (ViewSelector a) where
  negateG = fmap negateG

instance (Semigroup a) => Additive (ViewSelector a)

instance (Ord k) => PositivePart (ViewSelector (MonoidMap k SelectedCount)) where
  positivePart x =
    let u = mapMaybe standardPositivePart x
     in if u == mempty then Nothing else Just u

instance Filterable ViewSelector where
  mapMaybe f x =
    ViewSelector
      { _viewSelector_tasks = mapMaybe f (_viewSelector_tasks x)
      }

instance (Monoid a) => Query (ViewSelector a) where
  type QueryResult (ViewSelector a) = View a
  crop vs v =
    View
      { _view_tasks = if null (_viewSelector_tasks vs) then mempty else _view_tasks v
      }

newtype View a = View
  { _view_tasks :: Option a
  }
  deriving (Eq, Foldable, Functor, Generic, Show)

deriveJSON Json.defaultOptions 'View

makeLenses 'View

instance Monoid a => Semigroup (View a) where
  a <> b =
    View
      { _view_tasks = _view_tasks a <> _view_tasks b
      }

instance Monoid a => Monoid (View a) where
  mempty = View mempty
  mappend = (<>)

instance Filterable View where
  mapMaybe f x =
    View
      { _view_tasks = mapMaybe f (_view_tasks x)
      }
