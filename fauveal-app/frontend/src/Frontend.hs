{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Common.App
import Common.Route
import qualified Control.Foldl as L
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Scanl as SL
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Maybe (fromMaybe)
import Data.Semigroup (First (..), Option (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Numeric as T
import Obelisk.Configs (HasConfigs)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Generated.Static
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Rhyolite.Api (ApiRequest, public)
import Rhyolite.Frontend.App (AppWebSocket, RhyoliteWidget, functorToWire, runObeliskRhyoliteWidget, watchViewSelector)
import Text.Read (readMaybe)

frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = do
        elAttr "meta" ("charset" =: "UTF-8") blank
        elAttr "meta" ("name" =: "viewport" <> "contents" =: "width=device-width, initial-scale=1.0") blank
        elAttr "link" ("href" =: static @"styles.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
        elAttr "script" ("type" =: "module" <> "src" =: static @"bundle.min.js") blank,
      _frontend_body = prerender_ blank . fmap snd . runAppWidget "common/route" $ do
        divClass "content" $
          subRoute_ $ \case
            FrontendRouteMain -> mainView
    }

tshow :: Show a => a -> Text
tshow = Text.pack . show

svgXMLNamespace :: Namespace
svgXMLNamespace = "http://www.w3.org/2000/svg"

-- | Create an svg element with attributes and return the element
{-# INLINEABLE svgNamespaceElAttr' #-}
svgNamespaceElAttr' :: forall t m a. DomBuilder t m => Text -> Map Text Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
svgNamespaceElAttr' eltag attrs =
  Reflex.Dom.Core.element eltag $
    def
      & elementConfig_namespace ?~ svgXMLNamespace
      & elementConfig_initialAttributes .~ Map.mapKeys (AttributeName Nothing) attrs

svgAttr' ::
  forall t m a.
  DomBuilder t m =>
  Map Text Text ->
  m a ->
  m (Element EventResult (DomBuilderSpace m) t, a)
svgAttr' = svgNamespaceElAttr' "svg"

svgPathAttr' ::
  forall t m a.
  DomBuilder t m =>
  Map Text Text ->
  m a ->
  m (Element EventResult (DomBuilderSpace m) t, a)
svgPathAttr' = svgNamespaceElAttr' "path"

svgPathAttr :: forall t m a. DomBuilder t m => Map Text Text -> m a -> m a
svgPathAttr a = fmap snd . svgPathAttr' a

svgPaths :: DomBuilder t m => [Text] -> m ()
svgPaths = fmap mconcat . traverse (\p -> svgPathAttr ("stroke-linecap" =: "round" <> "stroke-linejoin" =: "round" <> "stroke-width" =: "2" <> "d" =: p) blank)

svgAttr :: forall t m a. DomBuilder t m => Map Text Text -> m a -> m a
svgAttr a = fmap snd . svgAttr' a

svgElDynAttr' ::
  forall t m a.
  (DomBuilder t m, PostBuild t m) =>
  Dynamic t (Map Text Text) ->
  m a ->
  m (Element EventResult (DomBuilderSpace m) t, a)
svgElDynAttr' =
  elDynAttrNS'
    (Just svgXMLNamespace)
    "svg"

slider ::
  forall m t.
  (DomBuilder t m) =>
  Float ->
  Float ->
  Float ->
  m (Dynamic t Text)
slider init min max =
  _inputElement_value
    <$> inputElement ele
  where
    ele :: InputElementConfig EventResult t (DomBuilderSpace m)
    ele =
      (def :: InputElementConfig EventResult t (DomBuilderSpace m)) & inputElementConfig_initialValue .~ tinit
        & inputElementConfig_elementConfig
          .~ ( def
                 & elementConfig_initialAttributes .~ ("type" =: "range" <> "min" =: tMin <> "max" =: tMax <> "step" =: "0.000001")
             )

    tMin = tshow min
    tMax = tshow max
    tinit = tshow init

svgBarOfColor ::
  (DomBuilder t m, Show a, Ord a, Num a) =>
  a ->
  a ->
  a ->
  a ->
  a ->
  m (Element EventResult (DomBuilderSpace m) t)
svgBarOfColor offset r' g' b' h = fst <$> svgNamespaceElAttr' "rect" (("width" =: "100%") <> ("height" =: tshow h) <> ("y" =: tshow offset) <> ("fill" =: ("rgba(" <> tshow r <> "," <> tshow g <> "," <> tshow b <> ",1)"))) blank
  where
    clamp l u x = max l $ min u x
    clamp' = clamp 0 255
    r = clamp' r'
    b = clamp' b'
    g = clamp' g'

textToFloat :: Text -> Maybe Float
textToFloat = readMaybe . T.unpack

mainView ::
  forall m t.
  ( HasApp t m,
    DomBuilder t m,
    PostBuild t m,
    MonadHold t m
  ) =>
  m ()
mainView = divClass "container mx-auto px-4 pt-12" $ do
  ss' <- divClass "grid grid-cols-4 gap-2" $ do
    replicateM
      8
      (slider 0 (negate 10) 10)

  let ss = mapMaybe textToFloat <$> distributeListOverDyn ss'
  let svgHeight :: Int = 466
  divClass "pt-12" $
    widgetHold_ blank $
      ffor (updated ss) $ \case
        [x0, x1, x2, x3, x4, x5, x6, x7] -> do
          now' <- getPostBuild

          let req = public (PublicRequest_VAE8 (Four (Two x0 x1) (Two x2 x3) (Two x4 x5) (Two x6 x7))) <$ now'
          res <- requestingIdentity req
          widgetHold_ blank $
            ffor res $ \case
              Nothing -> text "Nothing"
              Just fo -> void $ svgAttr ("height" =: tshow svgHeight <> "width" =: "100%") $ foldM renderColorRect 0 (scaleToZeroMax (realToFrac svgHeight / 15) $ fmap (fmap (* 0.01)) fo)
        _ -> text "Got more than eight values!"

sizeComponent :: Four a -> a
sizeComponent (Four _ _ _ s) = s

modifySize :: (t -> t) -> Four t -> Four t
modifySize f (Four r g b s) = Four r g b (f s)

maybeSizeComponent ::
  (Functor (p (Four b1)), Num b2, Profunctor p) =>
  p b1 (Maybe b2) ->
  p (Four b1) b2
maybeSizeComponent f = fromMaybe 0 <$> lmap sizeComponent f

minimumSize ::
  L.Fold
    (Four Float)
    Float
minimumSize = maybeSizeComponent L.minimum

maximumSize :: L.Fold (Four Float) Float
maximumSize = maybeSizeComponent L.maximum

maximumSizeOffset :: L.Fold (Four Float) Float
maximumSizeOffset = (+) <$> (abs <$> minimumSize) <*> maximumSize

scaleToZeroMax ::
  Float ->
  [Four Float] ->
  [Four Float]
scaleToZeroMax max' fs = modifySize xScaled <$> fs
  where
    (mn, mx) = L.fold ((,) <$> minimumSize <*> maximumSize) fs
    xStd x = (x - mn) / (mx - mn)
    xScaled x = xStd x * (max' - 0) + 0

renderColorRect :: DomBuilder t0 m => Float -> Four Float -> m Float
renderColorRect acc (Four r g b s) = do
  void $ svgBarOfColor acc r g b (s :: Float)
  pure $ acc + s

runAppWidget ::
  ( HasConfigs m,
    TriggerEvent t m,
    PerformEvent t m,
    MonadHold t m,
    PostBuild t m,
    MonadFix m,
    Prerender x t m
  ) =>
  Text ->
  RoutedT t (R FrontendRoute) (RhyoliteWidget (ViewSelector SelectedCount) (ApiRequest () PublicRequest PrivateRequest) t m) a ->
  RoutedT
    t
    (R FrontendRoute)
    m
    ( Dynamic
        t
        (AppWebSocket t (ViewSelector ())),
      a
    )
runAppWidget uri =
  runObeliskRhyoliteWidget
    functorToWire
    uri
    checkedFullRouteEncoder
    (BackendRouteListen :/ ())

type HasApp t m =
  ( MonadQuery t (ViewSelector SelectedCount) m,
    Requester t m,
    Request m ~ ApiRequest () PublicRequest PrivateRequest,
    Response m ~ Identity
  )
