{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Werror #-}

module Common.Prelude.Generics
  ( gFromEnum,
    gToEnum,
    GToEnum,
    gMinBound,
    gMaxBound,
    GBounded,
    galignWith,
    GAlignWith,
    gnil,
    GNil,
    gmapMaybe,
    GFilterable,
    gmapMaybeSnd,
    GFilterableSnd,
    Singular,
    singular,
  )
where

import Control.Lens (LensLike, _2)
import Data.Align (Align (..), Semialign (..))
-- for test
import Data.Char (isUpper)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.These (These (..))
import Data.Witherable (Filterable (..))
import GHC.Generics
  ( Generic (..),
    Generic1 (..),
    K1 (K1),
    M1 (M1),
    Par1 (Par1),
    Rec1 (Rec1),
    U1 (..),
    V1,
    type (:*:) (..),
    type (:+:) (..),
    type (:.:) (Comp1),
  )
import GHC.TypeLits (KnownNat, Nat, natVal, type (*), type (+))

-------------------------------------------------------------------------------
-- Enum
-------------------------------------------------------------------------------

-- | Get the total number of (non-bottom) inhabitants of type `a`.
size :: forall a. (KnownNat (Size a)) => Int
size = fromInteger (natVal (Proxy @(Size a)))

-- | The total number of (non-bottom) inhabitants of type `a`.
type family Size a :: Nat where
  Size U1 = 1
  Size (l :+: r) = Size l + Size r
  Size (l :*: r) = Size l * Size r
  Size (K1 i c) = Size (Rep c)
  Size (M1 i t c) = Size c

-- | Generic implementation of toEnum
class KnownNat (Size a) => GToEnum (a :: Type -> Type) where
  gToEnum' :: Int -> a p

instance GToEnum U1 where
  gToEnum' 0 = U1
  gToEnum' _ = error "gToEnum: bad argument"

instance (GToEnum l, GToEnum r, KnownNat (Size l + Size r)) => GToEnum (l :+: r) where
  gToEnum' i
    | i < size @l = L1 (gToEnum' i)
    | otherwise = R1 (gToEnum' (i - size @l))

instance (GToEnum l, GToEnum r, KnownNat (Size l * Size r)) => GToEnum (l :*: r) where
  gToEnum' i = (gToEnum' d :*: gToEnum' m)
    where
      (d, m) = i `divMod` size @r

instance (Generic c, GToEnum (Rep c)) => GToEnum (K1 i c) where
  gToEnum' i = K1 (to (gToEnum' i))

instance GToEnum c => GToEnum (M1 i t c) where
  gToEnum' i = M1 (gToEnum' i)

gToEnum :: (Generic a, GToEnum (Rep a)) => Int -> a
gToEnum = to . gToEnum'

-- | Generic implementation of fromEnum
class KnownNat (Size a) => GFromEnum (a :: Type -> Type) where
  gFromEnum' :: a p -> Int

instance GFromEnum U1 where
  gFromEnum' _ = 0

instance (GFromEnum l, GFromEnum r, KnownNat (Size l + Size r)) => GFromEnum (l :+: r) where
  gFromEnum' (L1 l) = gFromEnum' l
  gFromEnum' (R1 r) = size @l + gFromEnum' r

instance (GFromEnum l, GFromEnum r, KnownNat (Size l * Size r)) => GFromEnum (l :*: r) where
  gFromEnum' (l :*: r) = (gFromEnum' l * size @r) + gFromEnum' r

instance (Generic c, GFromEnum (Rep c)) => GFromEnum (K1 i c) where
  gFromEnum' (K1 c) = gFromEnum' (from c)

instance GFromEnum c => GFromEnum (M1 i t c) where
  gFromEnum' (M1 x) = gFromEnum' x

gFromEnum :: (Generic a, GFromEnum (Rep a)) => a -> Int
gFromEnum = gFromEnum' . from

-------------------------------------------------------------------------------
-- Bounded
-------------------------------------------------------------------------------

-- | Generic implementation of minBound and maxBound
class GBounded a where
  gMinBound' :: a p
  gMaxBound' :: a p

instance GBounded V1 where
  gMinBound' = undefined
  gMaxBound' = undefined

instance GBounded U1 where
  gMinBound' = U1
  gMaxBound' = U1

instance (GBounded l, GBounded r) => GBounded (l :+: r) where
  gMinBound' = L1 gMinBound'
  gMaxBound' = R1 gMaxBound'

instance (GBounded l, GBounded r) => GBounded (l :*: r) where
  gMinBound' = gMinBound' :*: gMinBound'
  gMaxBound' = gMaxBound' :*: gMaxBound'

instance (Bounded c) => GBounded (K1 i c) where
  gMinBound' = K1 $ minBound
  gMaxBound' = K1 $ maxBound

instance (GBounded c) => GBounded (M1 i t c) where
  gMinBound' = M1 $ gMinBound'
  gMaxBound' = M1 $ gMaxBound'

gMinBound :: (Generic a, GBounded (Rep a)) => a
gMinBound = to gMinBound'

gMaxBound :: (Generic a, GBounded (Rep a)) => a
gMaxBound = to gMaxBound'

-------------------------------------------------------------------------------
-- alignWith
-------------------------------------------------------------------------------

galignWith :: (Generic1 f, GAlignWith (Rep1 f)) => (These a b -> c) -> f a -> f b -> f c
galignWith f x y = to1 (galignWith' f (from1 x) (from1 y))

class Functor f => GAlignWith f where
  galignWith' :: (These a b -> c) -> f a -> f b -> f c

instance GAlignWith f => GAlignWith (M1 c i f) where
  galignWith' f (M1 x) (M1 y) = M1 (galignWith' f x y)

instance (GAlignWith f, GAlignWith g) => GAlignWith (f :*: g) where
  galignWith' f (x :*: y) (x' :*: y') = galignWith' f x x' :*: galignWith' f y y'

instance GAlignWith Par1 where
  galignWith' f (Par1 x) (Par1 y) = Par1 (f (These x y))

instance Align f => GAlignWith (Rec1 f) where
  galignWith' f (Rec1 x) (Rec1 y) = Rec1 (alignWith f x y)

-- instance (GAlignWith f, GAlignWith g) => GAlignWith (f :*: g) where
--     galignWith' f (x :*: y) (x' :*: y') = galignWith' f x x' :*: galignWith' f y y'

instance (Semialign f, GAlignWith g) => GAlignWith (f :.: g) where
  galignWith' f (Comp1 x) (Comp1 y) = Comp1 $ alignWith g x y
    where
      g (This ga) = fmap (f . This) ga
      g (That gb) = fmap (f . That) gb
      g (These ga gb) = galignWith' f ga gb

-------------------------------------------------------------------------------
-- nil
-------------------------------------------------------------------------------

gnil :: forall f a. (Generic1 f, GNil (Rep1 f)) => f a
gnil = to1 gnil'

class GNil f where
  gnil' :: f a

instance GNil f => GNil (M1 c i f) where
  gnil' = M1 gnil'

instance (GNil f, GNil g) => GNil (f :*: g) where
  gnil' = gnil' :*: gnil'

-- No Par1 instance
-- instance GNil Par1 where
--     gnil' = _

instance Align f => GNil (Rec1 f) where
  gnil' = Rec1 nil

instance Align f => GNil (f :.: g) where
  gnil' = Comp1 nil

-------------------------------------------------------------------------------
-- mapMaybe
-------------------------------------------------------------------------------

gmapMaybe :: (Generic1 f, GFilterable (Rep1 f)) => (a -> Maybe b) -> f a -> f b
gmapMaybe f = to1 . gmapMaybe' f . from1

class GFilterable f where
  gmapMaybe' :: (a -> Maybe b) -> f a -> f b

instance GFilterable f => GFilterable (M1 c i f) where
  gmapMaybe' f (M1 x) = M1 (gmapMaybe' f x)

instance (GFilterable f, GFilterable g) => GFilterable (f :*: g) where
  gmapMaybe' f (x :*: y) = gmapMaybe' f x :*: gmapMaybe' f y

-- No Par1 instance

instance Filterable f => GFilterable (Rec1 f) where
  gmapMaybe' f (Rec1 x) = Rec1 (mapMaybe f x)

instance (Filterable f, GFilterable g) => GFilterable (f :.: g) where
  gmapMaybe' f (Comp1 fg) = Comp1 (mapMaybe (Just . gmapMaybe' f) fg)

-------------------------------------------------------------------------------
-- mapMaybeSnd, for records with f (g (data, x)) like structure
-------------------------------------------------------------------------------

gmapMaybeSnd :: (Generic1 f, GFilterableSnd (Rep1 f)) => (a -> Maybe b) -> f a -> f b
gmapMaybeSnd f = to1 . gmapMaybeSnd' f . from1

class GFilterableSnd f where
  gmapMaybeSnd' :: (a -> Maybe b) -> f a -> f b

instance GFilterableSnd f => GFilterableSnd (M1 c i f) where
  gmapMaybeSnd' f (M1 x) = M1 (gmapMaybeSnd' f x)

instance (GFilterableSnd f, GFilterableSnd g) => GFilterableSnd (f :*: g) where
  gmapMaybeSnd' f (x :*: y) = gmapMaybeSnd' f x :*: gmapMaybeSnd' f y

-- we'll have a fields with types
--
-- f (x, a)
-- f $$ g (x, a)
-- f $$ g $$ h (x, a)
--
-- So we
-- - fmap the chains, until we have f :.: Rec1 ((,) x) a
-- - mapMaybe when we hit the bottom pair.
--
instance GFilterableSndComp f g => GFilterableSnd (f :.: g) where
  gmapMaybeSnd' f (Comp1 fg) = Comp1 (gmapMaybeSndComp f fg)

class GFilterableSndComp f g where
  gmapMaybeSndComp :: (a -> Maybe b) -> f (g a) -> f (g b)

instance (Filterable f, Singular g) => GFilterableSndComp f (Rec1 g) where
  gmapMaybeSndComp f = mapMaybe (singular f)

instance (Functor f, GFilterableSndComp g h) => GFilterableSndComp f (g :.: h) where
  gmapMaybeSndComp f = fmap $ \(Comp1 x) -> Comp1 $ gmapMaybeSndComp f x

-------------------------------------------------------------------------------
-- Singular
-------------------------------------------------------------------------------

-- https://oleg.fi/gists/posts/2018-05-12-singleton-container.html
--
-- Used here, so we don't accidentally traverse over not-pair.
class Traversable t => Singular t where
  singular :: Functor f => LensLike f (t a) (t b) a b

instance Singular f => Singular (Rec1 f) where
  singular f (Rec1 x) = Rec1 <$> singular f x
  {-# INLINE singular #-}

instance Singular ((,) x) where
  singular = _2
  {-# INLINE singular #-}

-------------------------------------------------------------------------------
-- Test
-------------------------------------------------------------------------------

-- TODO: put these in real tests
data T1 a = T1 a [a] deriving (Show, Generic1)

data T2 a = T2 (Maybe a) [a] deriving (Show, Generic1)

_test1 :: T1 (These Char Bool)
_test1 = galignWith id (T1 'x' "yz") (T1 True [])

_test2 :: T2 (These Char Bool)
_test2 = gnil

_test3 :: T2 Char
_test3 = gmapMaybe (\c -> if isUpper c then Just c else Nothing) $ T2 Nothing "FooBar"
