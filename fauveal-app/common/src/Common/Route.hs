{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Obelisk.Route
import Obelisk.Route.TH (deriveRouteComponent)

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRouteMissing :: BackendRoute ()
  -- | Websocket listen
  BackendRouteListen :: BackendRoute ()

data FrontendRoute :: * -> * where
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.
  FrontendRouteMain :: FrontendRoute ()

fullRouteEncoder ::
  Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder =
  mkFullRouteEncoder
    (FullRoute_Backend BackendRouteMissing :/ ())
    ( \case
        BackendRouteMissing -> PathSegment "missing" $ unitEncoder mempty
        BackendRouteListen -> PathSegment "listen" $ unitEncoder mempty
    )
    ( \case
        FrontendRouteMain -> PathEnd $ unitEncoder mempty
    )

checkedFullRouteEncoder :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedFullRouteEncoder = case checkEncoder fullRouteEncoder of
  Left e -> error (show e)
  Right x -> x

concat
  <$> traverse
    deriveRouteComponent
    [ ''BackendRoute,
      ''FrontendRoute
    ]
