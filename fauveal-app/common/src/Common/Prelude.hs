{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Common.Prelude
  ( module A,
    module Common.Prelude,
  )
where

import Common.Prelude.Generics as A
  ( GAlignWith,
    GBounded,
    GFilterable,
    GFilterableSnd,
    GNil,
    GToEnum,
    gFromEnum,
    gMaxBound,
    gMinBound,
    gToEnum,
    galignWith,
    gmapMaybe,
    gmapMaybeSnd,
    gnil,
  )
import Common.Prelude.Orphans ()
import Common.Prelude.UUID as A
import Control.Applicative as A (Const (..), liftA2)
import Control.DeepSeq as A (NFData (..), NFData1 (..))
import Control.Exception as A (Exception (..), SomeException (..))
import Control.Lens as A
  ( FoldableWithIndex (..),
    FunctorWithIndex (..),
    Lens',
    TraversableWithIndex (..),
    iforM,
    iforM_,
    view,
    (%~),
    (.~),
    (<&>),
    (?~),
    (^.),
    (^?),
  )
import Control.Monad as A
  ( forM,
    forM_,
    forever,
    guard,
    join,
    liftM2,
    mfilter,
    replicateM,
    replicateM_,
    unless,
    void,
    when,
    (<=<),
    (>=>),
  )
import Control.Monad.Base as A (MonadBase (..))
import Control.Monad.Catch as A (MonadCatch (..), MonadThrow (..))
import Control.Monad.Except as A (ExceptT (..), runExceptT)
import Control.Monad.IO.Class as A (MonadIO, liftIO)
import Control.Monad.Reader as A (MonadReader)
import Control.Monad.Trans as A (lift)
import Control.Monad.Trans.Control as A (MonadBaseControl (..))
import Data.Aeson as A
  ( FromJSON (parseJSON),
    FromJSON1,
    FromJSONKey,
    ToJSON (toJSON),
    ToJSON1,
    ToJSONKey,
    fromJSON,
    parseJSON1,
    toJSON1,
  )
import Data.Align as A (Align (..))
import Data.AppendMap as A (AppendMap)
import Data.Bifunctor as A (Bifunctor (..))
import Data.ByteString as A (ByteString)
-- not re-exported imports

import qualified Data.ByteString.Lazy as LBS
import Data.Coerce as A (Coercible, coerce)
import Data.Constraint as A (Dict (..))
import Data.Constraint.Extras as A (ArgDict (..))
import Data.Default as A (Default (..))
import Data.Dependent.Map as A (DMap)
import qualified Data.Dependent.Map as DMap (fromList, insertWith', toList)
-- import Data.Dependent.Map.Monoidal as A (MonoidalDMap)
import Data.Dependent.Sum as A (DSum (..), (==>))
import Data.Either as A (fromLeft, fromRight, isLeft, isRight, lefts, rights)
import Data.Either.Combinators as A (leftToMaybe, rightToMaybe)
import Data.Fixed (Centi)
import Data.Foldable as A (foldlM, for_, toList, traverse_)
import Data.Function as A (fix, (&))
import Data.Functor.Classes as A (Eq1 (..), Ord1 (..), Show1 (..), compare1, eq1, showsPrec1, showsUnaryWith)
import Data.Functor.Compose as A (Compose (..))
import Data.Functor.Identity as A (Identity (..))
import Data.Functor.Rep as A (Representable (..))
import Data.GADT.Compare as A (GCompare (..), GEq (..), GOrdering (..))
import Data.GADT.Show as A (GShow (..))
import Data.Int as A (Int16, Int32, Int64)
import Data.List as A (sortOn)
import Data.List.NonEmpty as A (NonEmpty (..), nonEmpty)
import Data.Map as A (Map)
import Data.Map.Monoidal as A (MonoidalMap (..))
import qualified Data.Map.Monoidal as MMap
import Data.Maybe as A (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Ord as A (Down (..))
import Data.Proxy as A (Proxy (..))
import Data.Semigroup as A (First (..), Min (..), Semigroup (..))
import Data.Semigroup.Generic as A (gmappend, gmempty)
import Data.Set as A (Set)
import Data.Some as A (Some (Some), withSome)
import Data.String as A (IsString (..))
import Data.Text as A (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.These as A (These (..), these)
import Data.Time as A
  ( Day (..),
    UTCTime (..),
    addDays,
    fromGregorian,
    fromGregorianValid,
    toGregorian,
    toModifiedJulianDay,
  )
import Data.Traversable as A (for, traverse)
import Data.Type.Equality as A ((:~:) (..))
import Data.Typeable as A (Typeable)
import Data.Universe.Class as A (Universe (..))
import Data.Witherable as A (Filterable (catMaybes, mapMaybe))
import Data.Word as A (Word64)
import GHC.Generics as A (Generic, Generic1)
import Generic.Data as A (Generically (..))
import Numeric.Natural as A (Natural)
import Reflex as A (ffor, fforMaybe)
import Reflex.FunctorMaybe as A (FunctorMaybe (..))
import Text.Read as A (readMaybe)

type (:.) f g = Compose f g

infix 5 :.

toCenti :: Real a => a -> Centi
toCenti = realToFrac

ffmap :: (Functor f2, Functor f1) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
ffmap = fmap . fmap

ttraverse ::
  (Traversable t1, Traversable t2, Applicative f) =>
  (a -> f b) ->
  t1 (t2 a) ->
  f (t1 (t2 b))
ttraverse = traverse . traverse

fffmap :: (Functor f3, Functor f2, Functor f1) => (a -> b) -> f3 (f1 (f2 a)) -> f3 (f1 (f2 b))
fffmap = fmap . fmap . fmap

(<<$>>) :: (Functor f2, Functor f1) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = ffmap

infixl 4 <<$>>

(<<&>>) :: (Functor f2, Functor f1) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<<&>>) = flip (<<$>>)

infixl 1 <<&>>

(<<$) :: (Functor f2, Functor f1) => a -> f1 (f2 b) -> f1 (f2 a)
v <<$ f = fmap (v <$) f

infixl 4 <<$

(<<*>>) :: (Applicative f, Applicative f1) => f (f1 (a -> b)) -> f (f1 a) -> f (f1 b)
(<<*>>) = liftA2 (<*>)

infixl 4 <<*>>

maskMempty :: (Monoid m, Eq m) => m -> Maybe m
maskMempty m = if m == mempty then Nothing else Just m

unmaskMempty :: (Monoid m, Eq m) => Maybe m -> m
unmaskMempty (Just m) = m
unmaskMempty Nothing = mempty

bounded :: (Enum a, Bounded a) => [a]
bounded = [minBound .. maxBound]

-- | @Lens@ into 'Representable'.
--
-- @
-- 'rix' :: ('Representable' f, Eq ('Rep' f)) => 'Rep' f -> Lens' (f a) a
-- @
--
-- From: https://github.com/ekmett/adjunctions/pull/55
rix :: (Functor g, Representable f, Eq (Rep f)) => Rep f -> (a -> g a) -> f a -> g (f a)
rix i agb fa = setter <$> agb (index fa i)
  where
    setter b = tabulate $ \i' ->
      if i == i'
        then b
        else index fa i'
{-# INLINE rix #-}

-- | Our either combinator package is too old to have this.
maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing = Left y

-- | 'show' into 'Text'.
tshow :: Show a => a -> Text
tshow = T.pack . show

readByteString :: Read a => ByteString -> Maybe a
readByteString = readMaybe <=< fmap T.unpack . rightToMaybe . TE.decodeUtf8'

-- | meet from @lattices@ package.
--
-- >>> not /\ id $ True
-- False
(/\) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f /\ g = \x -> f x && g x

infixr 6 /\

-- | join from @lattices@ package.
--
-- >>> not \/ id $ True
-- True
(\/) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f \/ g = \x -> f x || g x

infixr 5 \/

-------------------------------------------------------------------------------
-- Text UTF8 decoding
-------------------------------------------------------------------------------

decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = decodeUtf8With lenientDecode

decodeUtf8LenientLazy :: LBS.ByteString -> LT.Text
decodeUtf8LenientLazy = LT.decodeUtf8With lenientDecode

-------------------------------------------------------------------------------
-- Type level $
-------------------------------------------------------------------------------

type f $$ x = f x

infixr 0 $$

-------------------------------------------------------------------------------
-- dependent-sum and -map
-------------------------------------------------------------------------------

-- | Constructor synonym for dependent sums that eliminates the 'Identity'
-- newtype wrapper.  This is similar to ('==>') except that it has a more
-- restricted type and it can be used in pattern contexts.
pattern (:=>:) :: tag a -> a -> DSum tag Identity
pattern d :=>: v = d :=> Identity v

{-# COMPLETE (:=>:) #-}

-- | Map (hoist) functor in 'Some'.
mapSome :: (forall x. f x -> g x) -> Some f -> Some g
mapSome nt (Some fx) = Some (nt fx)

dmapKeysMaybe :: GCompare k' => (forall a. k a -> Maybe (k' a)) -> DMap k f -> DMap k' f
dmapKeysMaybe f = DMap.fromList . mapMaybe (\(k :=> a) -> flip (:=>) a <$> f k) . DMap.toList

insertDSumWith ::
  GCompare k2 =>
  DMap k2 f ->
  (forall v. f v -> f v -> f v) ->
  DSum k2 f ->
  DMap k2 f
insertDSumWith dm f (k :=> fv) = DMap.insertWith' f k fv dm

insertDSum ::
  forall k (k2 :: k -> *) (f :: k -> *).
  GCompare k2 =>
  DMap k2 f ->
  DSum k2 f ->
  DMap k2 f
insertDSum dm = insertDSumWith dm const

-- To workaround
-- https://gitlab.haskell.org/ghc/ghc/issues/15681
withSomeM :: Monad m => m (Some tag) -> (forall a. tag a -> m b) -> m b
withSomeM m k = m >>= \s -> withSome s k

-- | foldMap with effects.  From rio: https://hackage.haskell.org/package/rio-0.1.12.0/docs/src/RIO.Prelude.Extra.html#foldMapM
--  Uses strict left fold.
foldMapM ::
  (Monad m, Monoid w, Foldable t) =>
  (a -> m w) ->
  t a ->
  m w
foldMapM f =
  A.foldlM
    ( \acc a -> do
        w <- f a
        return $! mappend acc w
    )
    mempty

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- | `when` for the case when you can give back a monoid
monoidalWhen :: (Applicative f, Monoid m) => Bool -> f m -> f m
monoidalWhen p s = if p then s else pure mempty
{-# INLINEABLE monoidalWhen #-}

fmapMaybeSnd :: Filterable f => (t -> Maybe b) -> f (a, t) -> f (a, b)
fmapMaybeSnd f = mapMaybe $ \(a, c) -> case f c of
  Nothing -> Nothing
  Just b -> Just (a, b)
{-# INLINEABLE fmapMaybeSnd #-}

-- | Check whether value is mempty, potentially faster than @== 'mempty'@.
--
-- @
-- 'isMempty' 'mempty' = True
-- 'isMempty' x        = False, x /= 'mempty'
-- @
class Monoid a => IsMempty a where
  isMempty :: a -> Bool

instance IsMempty [a] where isMempty = null

instance (Semigroup a) => IsMempty (Maybe a) where isMempty = null

instance (Ord k, Semigroup v) => IsMempty (MMap.MonoidalMap k v) where isMempty = MMap.null

ifM :: Monad m => m Bool -> m b -> m b -> m b
ifM c a b = do
  c' <- c
  if c' then a else b
