{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
-- TODO: Reflex deprecation warnings
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module Frontend.Common
  ( module Frontend.Common,
    Locale (..),
  )
where

-- import Frontend.Tailwind.Widget (AppUI, UI, clickableElClass)
-- import Frontend.Widget (MonadAppWidget)

import Common.App
import Common.Prelude
import Common.Route
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader)
import Data.Bool (bool)
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Diagrams hiding (text)
import Diagrams.Backend.SVG
import Frontend.I18n (HasI18n (localizeWith), HasLocale (askLocale), Locale)
import Graphics.Svg.Core hiding (Element)
import Obelisk.Route (R, renderBackendRoute, pattern (:/))
import Obelisk.Route.Frontend (SetRoute)
import Reflex (MonadHold, traceEvent)
import Reflex.Dom.Core
import Rhyolite.Frontend.App
  ( MonadRhyoliteFrontendWidget,
    MonadRhyoliteWidget,
  )

type MonadAppWidget t m = MonadRhyoliteWidget (ViewSelector SelectedCount) (Const ()) t m

type MonadAppFrontendWidget t m = MonadRhyoliteFrontendWidget (ViewSelector SelectedCount) (Const ()) t m

type UI t m =
  ( MonadHold t m,
    TriggerEvent t m,
    PerformEvent t m,
    DomBuilder t m,
    PostBuild t m,
    Prerender t m,
    MonadFix m,
    Reflex t
  )

type AppUI e t m =
  ( UI t m,
    Prerender t m,
    PostBuild t m,
    MonadReader e m,
    PerformEvent t m,
    TriggerEvent t m,
    DomBuilder t m,
    MonadAppWidget t m,
    SetRoute t (R FrontendRoute) m,
    -- Routed t (R FrontendRoute) m,
    HasLocale t Locale m,
    MonadHold t m
  )

buttonWithClass :: (DomBuilder t m) => Text -> m () -> m (Event t ())
buttonWithClass c t = do
  (e, _) <- elClass' "button" c t
  return $ domEvent Click e

clickableElClass :: DomBuilder t m => Text -> Text -> m b -> m (Event t ())
clickableElClass t c m = do
  (e, _) <- elClass' t c m
  return $ domEvent Click e

clickableElDynClass :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t Text -> m b -> m (Event t ())
clickableElDynClass t c m = do
  (e, _) <- elDynClass' t c m
  return $ domEvent Click e

buttonWithDynClass :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m () -> m (Event t ())
buttonWithDynClass c t = do
  (e, _) <- elDynClass' "button" c t
  return $ domEvent Click e

loader :: DomBuilder t m => m ()
loader =
  divClass "p-20 justify-center" loader'

loader' :: DomBuilder t m => m ()
loader' = divClass "text-5xl animate-spin bg-gray-700 border-2 border-gray-500 text-white px-9 p-2 rounded-full w-1" blank

-- | An uninterruptable fullscreenn loader.
fullLoader :: DomBuilder t m => m ()
fullLoader = divClass "w-full h-full fixed block top-0 left-0 bg-white opacity-75 z-50" $ elClass "span" "text-green-500 opacity-75 top-1/2 my-0 mx-auto block relative w-0 h-0" loader

dynText_ ::
  forall locale term t m.
  ( DomBuilder t m,
    PostBuild t m,
    HasI18n locale term Text,
    HasLocale t locale m
  ) =>
  Dynamic t term ->
  m ()
dynText_ = dynText'_ localizeWith

dynText'_ ::
  (PostBuild t1 m, HasLocale t1 locale m, DomBuilder t1 m) =>
  (locale -> a -> Text) ->
  Dynamic t1 a ->
  m ()
dynText'_ f termDyn = do
  localeDyn <- askLocale
  void $
    dyn $
      ffor localeDyn $ \locale ->
        dynText (f locale <$> termDyn)

dynFieldText ::
  (MonadWidget t m, HasLocale t locale m) =>
  (locale -> Const Text b) ->
  m ()
dynFieldText f = do
  localeDyn <- askLocale
  void $
    dyn $
      ffor localeDyn $ \locale ->
        constText (f locale)

-- | Generates a clickable text element wrapped in a div with the provided classes
clickDynText ::
  forall locale term t m.
  ( DomBuilder t m,
    PostBuild t m,
    HasI18n locale term Text,
    HasLocale t locale m,
    Show term,
    MonadHold t m
  ) =>
  -- | `div` classes
  Text ->
  -- | The term to localize
  Dynamic t term ->
  m (Event t term)
clickDynText c termDyn = do
  localeDyn <- askLocale
  clickEE <- dyn $
    ffor (zipDyn localeDyn termDyn) $ \(locale, term) -> do
      clickE <- traceEvent "clickableElClass" <$> clickableElClass "a" c (dynText (localizeWith locale <$> termDyn))
      pure $ traceEvent "clickDynText" $ term <$ clickE
  switchHold never clickEE

localizedPlaceholder ::
  (Reflex t, HasI18n locale term Text) =>
  Dynamic t locale ->
  term ->
  Dynamic t (Map Text Text)
localizedPlaceholder localeDyn term =
  ffor localeDyn $ \locale -> "placeholder" =: localizeWith locale term

-- TODO: Awaiting PR: https://github.com/reflex-frp/reflex-dom/pull/211

-- | Like 'elDynAttr'' but allows you to modify the element configuration.
elDynAttrWithModifyConfig' ::
  forall t m a.
  (DomBuilder t m, PostBuild t m) =>
  ( ElementConfig EventResult t (DomBuilderSpace m) ->
    ElementConfig EventResult t (DomBuilderSpace m)
  ) ->
  Text ->
  Dynamic t (Map Text Text) ->
  m a ->
  m (Element EventResult (DomBuilderSpace m) t, a)
elDynAttrWithModifyConfig' f elementTag attrs child = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  let cfg =
        def & modifyAttributes .~ fmapCheap mapKeysToAttributeName modifyAttrs
  result <- element elementTag (f cfg) child
  postBuild <- getPostBuild
  notReadyUntil postBuild
  pure result

-- | Like 'elDynAttr'' but configures "prevent default" on the given event.
elDynAttrWithPreventDefaultEvent' ::
  forall en t m a.
  (DomBuilder t m, PostBuild t m) =>
  -- | Event on the element to configure with 'preventDefault'
  EventName en ->
  -- | Element tag
  Text ->
  -- | Element attributes
  Dynamic t (Map Text Text) ->
  -- | Child of element
  m a ->
  m (Element EventResult (DomBuilderSpace m) t, a) -- An element and the result of the child
elDynAttrWithPreventDefaultEvent' ev =
  elDynAttrWithModifyConfig'
    ( \elCfg ->
        elCfg & elementConfig_eventSpec
          %~ addEventSpecFlags
            (Proxy :: Proxy (DomBuilderSpace m))
            ev
            (const preventDefault)
    )

-- Tag each response with the original request value.
requestingIdentityTagged ::
  (Response m ~ Identity, Requester t m, MonadHold t m) =>
  (a -> Request m b) ->
  Event t a ->
  m (Event t (a, b))
requestingIdentityTagged mkRequest evt = do
  reqBehaviour <- hold Nothing (Just <$> evt)
  respEvt <- requestingIdentity (mkRequest <$> evt)
  return $ attachWithMaybe (\a b -> (,b) <$> a) reqBehaviour respEvt

-- | Convenience function for workflows returning ()
pureWf :: Applicative f => a -> f ((), a)
pureWf = pureWf' ()

pureWf' :: Applicative f => b -> a -> f (b, a)
pureWf' b = pure . (,) b

template :: DomBuilder t m => m a -> m a
template = id

-- | Add a standard back button and page title, where the page may
-- switch to a subsequent workflow.
wrapBack ::
  (UI t m) =>
  -- | Back event
  Event t () ->
  -- | Back button target
  Workflow t m () ->
  -- | Page content, which may return a new workflow to switch to
  m (Event t (Workflow t m ())) ->
  Workflow t m ()
wrapBack back x m = Workflow $
  template $ do
    choice <- m
    pureWf $ leftmost [x <$ back, choice]

-- | Add a standard back button returning a give a, and page title, where the page may
-- switch to a subsequent workflow.
wrapBackWith ::
  (UI t m) =>
  a ->
  -- | Back event
  m (Event t ()) ->
  -- | Back button target
  Workflow t m a ->
  -- | Page content, which may return a new workflow to switch to
  m (Event t (Workflow t m a)) ->
  Workflow t m a
wrapBackWith last' back x m = Workflow $
  template $ do
    back' <- back
    choice <- m
    pureWf' last' $ leftmost [x <$ back', choice]

-- | Add a standard back button and page title, where the only option
-- is to go back.
wrapBack' ::
  (UI t m) =>
  -- | Back event
  Event t () ->
  -- | Back button target
  Workflow t m () ->
  -- | Page content
  m () ->
  Workflow t m ()
wrapBack' back x m = Workflow $
  template $ do
    m
    pureWf $ x <$ back

-- | Run a workflow returning a switched Event.
workflowE :: (Adjustable t m, MonadHold t m, MonadFix m) => Workflow t m (Event t a) -> m (Event t a)
workflowE = fmap (switch . current) . workflow

-- | Run a workflow returning a switched Dynamic.
workflowD :: (Adjustable t m, MonadHold t m, MonadFix m) => Workflow t m (Dynamic t a) -> m (Dynamic t a)
workflowD = fmap join . workflow

mapWorkflowNT ::
  (Functor g, Reflex t) =>
  (forall x. f x -> g x) ->
  Workflow t f a ->
  Workflow t g a
mapWorkflowNT f (Workflow wf) =
  Workflow $ second (fmap (mapWorkflowNT f)) <$> f wf

-- | A static, random, square, grayscale image
picsomeSeededGrey :: DomBuilder t m => Int -> Text -> Map Text Text -> m ()
picsomeSeededGrey size seed c = elAttr "img" ("src" =: ("https://picsum.photos/seed/" <> seed <> "/" <> tshow size <> "?grayscale") <> c) blank

picsomeIdGrey :: DomBuilder t m => Int -> Int -> Map Text Text -> m ()
picsomeIdGrey size id' c = elAttr "img" ("src" =: ("https://picsum.photos/id/" <> tshow id' <> "/" <> tshow size <> "?grayscale") <> c) blank

placeHolderAvatar :: DomBuilder t m => Map Text Text -> m ()
placeHolderAvatar = picsomeIdGrey 200 1025

avatarClasses :: Map Text Text
avatarClasses = avatarClasses' 20

avatarClasses' :: Natural -> Map Text Text
avatarClasses' w = "class" =: ("w-" <> tshow w <> " h- " <> tshow w <> "  object-cover rounded-full shadow cursor-pointer")

svgXMLNamespace :: Namespace
svgXMLNamespace = "http://www.w3.org/2000/svg"

-- | Create an svg element with attributes and return the element
{-# INLINEABLE svgNamespaceElAttr' #-}
svgNamespaceElAttr' :: forall t m a. DomBuilder t m => Text -> Map Text Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
svgNamespaceElAttr' eltag attrs =
  element eltag $
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

outerDiv :: DomBuilder t m => m a -> m a
outerDiv =
  divClass "min-w-screen min-h-screen items-center justify-center flex flex-col bg-white px-5 pt-6"

outerDivFull :: DomBuilder t m => m a -> m a
outerDivFull =
  outerDiv

toggleComponent :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Maybe Text -> m (Dynamic t Bool)
toggleComponent mbLabel =
  elClass "label" "flex items-center relative w-max cursor-pointer select-none" $ mdo
    maybe blank (elClass "span" "text-lg font-bold mr-3" . text) mbLabel
    (e, _) <- elDynAttr' "input" ((\v -> "type" =: "checkbox" <> "class" =: bool baseClassesInput onInput v) <$> isChecked) blank
    elDynAttr "span" (bool ("class" =: offSpan) ("class" =: onSpan) <$> isChecked) blank
    isChecked <- toggle False $ domEvent Click e
    pure isChecked
  where
    baseClassesInput = "appearance-none transition-colors cursor-pointer w-14 h-7 rounded-full focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-black focus:ring-blue-500 bg-gray-100"
    onClassesInput = "bg-green-500"
    onInput = T.unwords [baseClassesInput, onClassesInput]
    baseClassesSpan = "w-7 h-7 right-7 absolute rounded-full transform transition-transform"
    offClassesSpan = "bg-gray-200"
    onClassesSpan = "bg-gray-200 translate-x-7"
    onSpan = T.unwords [baseClassesSpan, onClassesSpan]
    offSpan = T.unwords [baseClassesSpan, offClassesSpan]

hoverClass :: Text
hoverClass = "hover:text-pink-600"

makeSelectable ::
  ( DomBuilder t f,
    PostBuild t f,
    HasI18n locale term Text,
    HasLocale t locale f
  ) =>
  term ->
  f (Event t term)
makeSelectable = makeSelectable' "h3"

makeSelectable' ::
  ( DomBuilder t f,
    PostBuild t f,
    HasI18n locale term Text,
    HasLocale t locale f
  ) =>
  Text ->
  term ->
  f (Event t term)
makeSelectable' c s = (s <$) <$> clickableElClass c (textClasses <> " " <> hoverClass) (dynText_ $ constDyn s)

makeSelectableWithCarrot ::
  ( DomBuilder t f,
    PostBuild t f,
    HasI18n locale term Text,
    HasLocale t locale f,
    MonadHold t f,
    MonadFix f
  ) =>
  term ->
  f (Event t term)
makeSelectableWithCarrot s = mdo
  termE <-
    (s <$)
      <$> clickableElClass
        "h3"
        (textClasses <> " " <> hoverClass)
        ( divClass "grid grid-cols2" $ do
            divClass "justify-self-start col-start-1" . dynText_ $ constDyn s
            divClass "justify-self-end col-start-2" . widgetHold_ (rightChevron chevClassRot) $
              ffor (updated isClickedD) $
                \isClicked -> if isClicked then rightChevron chevClass else rightChevron chevClassRot
        )
  isClickedD <- toggle False termE
  pure termE
  where
    chevClasses = "class" =: "w-10 h-10 transition ease-out duration-300 hover:text-pink-600 text-pink-500" <> "fill" =: "none" <> "viewBox" =: "0 0 24 24" <> "stroke" =: "grey"
    chevClass = chevClasses
    chevClassRot = chevClasses <> "transform" =: "rotate(90)"

svgComponentClasses :: Map Text Text
svgComponentClasses = "class" =: "w-16 h-16 transition ease-out duration-300 hover:text-pink-600 text-pink-500" <> "fill" =: "none" <> "viewBox" =: "0 0 24 24" <> "stroke" =: "currentColor"

rightChevron :: DomBuilder t m => Map Text Text -> m ()
rightChevron c = svgAttr c $ svgPaths ["M9.293 12.95l.707.707L15.657 8l-1.414-1.414L10 10.828 5.757 6.586 4.343 8z"]

textClasses' :: Text
textClasses' = "font-mono transition ease-out duration-300"

textClasses :: Text
textClasses = textClasses' <> " text-2xl"

largeTextClasses :: Text
largeTextClasses = textClasses' <> " text-4xl"

pureEvent :: Reflex t => a -> Event t a
pureEvent = updated . constDyn

makeToggleable ::
  ( MonadWidget t f,
    MonadAppWidget t f,
    AppUI e t f,
    HasI18n Locale term Text
  ) =>
  term ->
  f (Dynamic t term)
makeToggleable s = do
  elClass "h3" textClasses $ dynText_ $ constDyn s
  (s <$) <$> toggleComponent Nothing

table,
  thead,
  tr,
  tbody,
  td,
  th ::
    ( MonadWidget t m
    ) =>
    Text ->
    m a ->
    m a
table = elClass "table"
thead = elClass "thead"
tr = elClass "tr"
tbody = elClass "tbody"
td = elClass "td"
th = elClass "th"

constText :: MonadWidget t m => Const Text b -> m ()
constText = text . getConst

maybeBlankTd ::
  MonadWidget t m =>
  (a -> m ()) ->
  Maybe a ->
  m ()
maybeBlankTd f = td mempty . maybe blank f

coloredAlert :: DomBuilder t m => Text -> Text -> m b -> m b
coloredAlert color title body = elAttr "div" ("role" =: "alert") $ do
  divClass ("bg-" <> color <> "-500 text-white font-bold rounded-t px-4 py-2") . text $ title
  divClass ("border border-t-0 border-" <> color <> "-400 rounded-b bg-" <> color <> "-100 px-4 py-3 text-" <> color <> "-700") . el "p" $ body

notaBeneAlert :: DomBuilder t m => Text -> m () -> m ()
notaBeneAlert = coloredAlert "pink"

errorAlert :: DomBuilder t m => Text -> m () -> m ()
errorAlert = coloredAlert "red"

dynTextErrorAlert_,
  dynTextWarnAlert_,
  dynTextNoteAlert_ ::
    ( AppUI e t m,
      MonadWidget t m,
      MonadAppWidget t m,
      HasI18n Locale term Text
    ) =>
    term ->
    m ()
dynTextErrorAlert_ = errorAlert "Error" . dynText_ . pure
dynTextNoteAlert_ = notaBeneAlert "N.B." . dynText_ . pure

warnAlert :: DomBuilder t m => Text -> m () -> m ()
warnAlert = coloredAlert "yellow"

dynTextWarnAlert_ = warnAlert "Warning" . dynText_ . pure

data WithBorder = WithBorder | WithShadow | None

diagramToBase64Image :: (DomBuilder t m) => WithBorder -> Options SVG V2 Double -> Diagram B -> m ()
diagramToBase64Image bord opts diag = asImgBase64 . BSL.toStrict $ renderBS rendered
  where
    rendered = renderDia SVG opts diag
    ifBord = case bord of
      WithBorder -> "class" =: "border border-1 border-black"
      WithShadow -> "class" =: "shadow-lg"
      None -> mempty
    asImgBase64 :: (DomBuilder t m) => ByteString -> m ()
    asImgBase64 b = elAttr "img" (ifBord <> "src" =: ("data:image/svg+xml;base64," <> b')) blank
      where
        b' = T.decodeUtf8 $ Data.ByteString.Base64.encode b