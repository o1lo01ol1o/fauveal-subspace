{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Data.Functor.Poly where

import Common.Prelude (Const, Natural)
import Control.Lens (lens, set, view)
import qualified Control.Lens as Control.Lens.Getter
import Data.Dependent.Sum
import Data.Functor (Functor)
import Data.Functor.Identity (Identity (Identity))
import Data.Functor.Product
import Data.Functor.Sum
import Data.Void (Void)
import GHC.Base (Type)
import Prelude hiding (Functor, (**))

-- record Arena : Set where -- an arena consists of
-- field -- two fields
-- pos : Set -- one called pos, a set; and
-- dir : pos -> Set -- one called dir,
-- -- a set for each element of the set pos

data Arena :: Type -> Type -> Type

-- record Lens (p : Arena) (q : Arena) : Set where
-- field
-- observe : (pos p) -> (pos q)
-- interpret : (i : pos p) -> dir q (observe i) -> dir p i
-- interpret means pos -> ((pos' -> arr') $ pos)' -> arr

class Poly a where
  type Pos a
  type Dir a

class (Poly a, Poly c) => PolyLens a c where
  data Lens a c
  type Factor a c

instance Poly (Arena pos arr) where
  type Pos (Arena pos arr) = pos
  type Dir (Arena pos arr) = arr

instance (Poly p, Poly q) => PolyLens p q where
  type Factor p q = DSum LenseFactorization Identity
  data Lens p q = Lens
    { observe :: Pos p -> Pos q,
      interpret :: Pos p -> Dir q -> Dir p
    }

idLens :: Lens a a
idLens = Lens id (const id)

--positions as output and
--distinctions as input
-- arenaIO :: arr -> pos -> Arena pos arr
-- arenaIO _ _ =

type Self s = Arena s s

type Closed = Arena () ()

--
type Exception t = Arena Void t

type Emitter t = Arena () t

type Sensor t = Arena t

data DPair a p :: Type where
  MkDPair :: x -> p x -> DPair a p

-- type (**) f a = DSum f Identity

-- (**) :: a -> tag a -> DSum tag Identity
-- (**) a f = f :=> Identity a

data LenseFactorization a where
  LenseFactorization :: LenseFactorization (arenaA, Lens b (Dir arenaA), Lens (pos arenaA) b)

-- Factor :  {a, c : Arena} -> Lens a c -> (b : Arena ** (Lens b c, Lens a b))
-- x : a ** p
-- factor :: Lens a c -> DSum LenseFactorization Identity
-- factor f =
--   let vertf = Lens id (interpret f)
--       cartf = Lens (observe f) (const id)
--    in ((Arena (dir c . observe f), cartf, vertf) ** LensTag)

(<.>) :: Lens a2 a3 -> Lens a1 a2 -> Lens a1 a3
(<.>) lens23 lens12 = Lens obs int
  where
    obs = observe lens23 . observe lens12
    int p = interpret lens12 p . interpret lens23 (observe lens12 p)

infixr 4 <.>

-- liftArr :: (forall x. x -> f x) -> Arena pos arr -> Arena pos (f arr)
-- liftArr f ar = Arena fdis
--   where
--     fdis = f . dir ar

-- Bleh?
-- liftLens :: Functor f => Lens (Arena posA arrA) (Arena posB arrB) -> Lens (Arena posA (f arrA)) (Arena posB (f arrB))
-- liftLens lens = Lens (observe lens) int
--   where
--     int = fmap . interpret lens

-- extract :: Monad m => Lens (Arena _ _) (Arena _ _)
-- extract = Lens id pur
--   where
--     pur = pure

type Zero = Arena Void Void

infixr 4 <++>, <**>

type family (<++>) a b where
  (<++>) (Arena pos1 arr) (Arena pos2 arr) = Arena (Either pos1 pos2) arr

-- (<++>) :: Arena -> Arena -> Arena
-- (<++>) :: Arena pos1 arr -> Arena pos2 arr -> Arena (Either pos1 pos2) arr
-- (<++>) a b =
--   Arena
--     ( \case
--         Left p -> dir a p
--         Right p -> dir b p
--     )

-- sum : (ind : Type ** ind -> Arena) -> Arena
-- sum ::
--   DPair a1 (p :: a1 -> Arena a b) -> Arena (DPair a2 p2) arr
-- sum :: (t, t -> Arena pos arr) -> Arena (t, pos) arr
-- sum (_ind, arena) = Arena (\(i, p) -> dir (arena i) p)

sumLens :: Lens (Arena posa1 arra) (Arena posb1 arrb) -> Lens (Arena posa2 arra) (Arena posb2 arrb) -> Lens (Arena posa1 arra <++> Arena posa2 arra) (Arena posb1 arrb <++> Arena posb2 arrb)
sumLens l1 l2 = Lens o i
  where
    -- o : pos (a1 <++> a2) -> pos (b1 <++> b2)
    o (Left p1) = Left (observe l1 p1)
    o (Right p2) = Right (observe l2 p2)
    -- i :: (p : pos (a1 <++> a2)) -> dis (b1 <++> b2) (o p) -> dis (a1 <++> a2) p
    i (Left p1) d1 = interpret l1 p1 d1
    i (Right p2) d2 = interpret l2 p2 d2

copair :: Lens (Arena posa1 arra) b -> Lens (Arena posa2 arra) b -> Lens (Arena posa1 arra <++> Arena posa2 arra) b
copair l1 l2 = Lens obs int
  where
    -- obs :: pos (a1 <++> a2) -> pos b
    -- int :: (p : pos (a1 <++> a2)) -> dis b (obs p) -> dis (a1 <++> a2) p
    obs (Left p1) = observe l1 p1
    obs (Right p2) = observe l2 p2
    int (Left p1) d1 = interpret l1 p1 d1
    int (Right p2) d2 = interpret l2 p2 d2

type family (<**>) a b where
  (<**>) (Arena pos1 arr1) (Arena pos2 arr2) = Arena (pos1, pos2) (Either arr1 arr2)

prodLens ::
  Lens (Arena posa1 arra) (Arena posb1 arrb) ->
  Lens (Arena posa2 arra) (Arena posb2 arrb) ->
  Lens
    (Arena posa1 arra <**> Arena posa2 arra)
    (Arena posb1 arrb <**> Arena posb2 arrb)
prodLens l1 l2 = Lens o i
  where
    o (p1, p2) = (observe l1 p1, observe l2 p2)
    i (p1, _p2) (Left d1) = Left (interpret l1 p1 d1)
    i (_p1, p2) (Right d2) = Right (interpret l2 p2 d2)

pair ::
  Lens a (Arena posb1 arrb) ->
  Lens a (Arena posb2 arrb) ->
  Lens a (Arena posb1 arrb <**> Arena posb2 arrb)
pair l1 l2 = Lens obs int
  where
    obs p = (observe l1 p, observe l2 p)
    int p (Left d1) = interpret l1 p d1
    int p (Right d2) = interpret l2 p d2

type family (<@@>) a b where
  (<@@>) (Arena pos1 arr1) (Arena arr1 arr2) = Arena pos1 arr2

-- infix 4 **

-- (**) a b = b

-- circLens ::
--   forall posa1 posb1 arrb arra.
--   Lens (Arena posa1 arra) (Arena posb1 arrb) ->
--   Lens (Arena arra arra) (Arena arrb arrb) ->
--   Lens
--     (Arena posa1 arra <@@> Arena arra arra) -- p
--     (Arena posb1 arrb <@@> Arena arrb arrb) -- q
-- circLens l1 l2 = Lens o i
--   where
--     -- d2 = id
--     -- f = id
--     -- o :: arra -> arrb
--     o p = observe l2 (interpret l1 p) -- (observe l1 p ** (observe l2) . f . (interpret l1 p))
--     -- i :: arra -> arrb -> arra
--     i a = interpret l2 a
--       where
--         -- e1 : dis a1 p
--         e1 = interpret l1 p d1

-- where
--   e1 :: arra
--   e1 = interpret l1 p d1

-- Ex 3.2 from https://topos.site/poly-book.pdf
data A = Orange | Green deriving stock (Show)

data B = B1 | B2 deriving stock (Show)

data Sm = S1 | S2 | S3 deriving stock (Show)

mooreMachine :: Lens (Arena Sm B) (Arena Sm A)
mooreMachine = Lens go goLens
  where
    goLens S1 Orange = B1
    goLens S1 Green = B2
    goLens S2 Orange = B1
    goLens S2 Green = B2
    goLens S3 Green = B2
    goLens S3 Orange = B1

    -- arena = Arena arenaDir
    -- arenaDir S1 = B2
    -- arenaDir S2 = B1
    -- arenaDir S3 = B2
    go = id

-- auto' :: (Lens (Arena s b) (Arena s a), Arena s b) -> a -> (Arena s b, b)
-- auto' (mm, ar) a = (Arena (dir ar), pos')
--   where
--     pos_ = observe mm $  ar
--     pos' = interpret mm (pos ar) a

-- | >>>snd <$> auto [Orange, Orange, Green, Orange]
-- [B2,B1,B1,B2,B1]
-- auto :: [A] -> [(Arena Sm B, B)]
-- auto = ((init', initB) :) . snd . foldr go (init', mempty)
--   where
--     initB = dir init' (pos init')
--     go a (are, acc) =
--       let (are', b') = auto' (l, are) a
--        in (are', (are', b') : acc)
--     (l, init') = mooreMachine

-- _⊎ₚ_ : Poly → Poly → Poly
-- p ⊎ₚ q = record { pos = pos p ⊎ pos q ; dir = λ { (inj₁ x) → (dir p) x
--                                                 ; (inj₂ y) → (dir q) y } }
-- box p q = Arena (pos p) (pos q)

-- Definition 3.17 (Dependent dynamical systems). A dependent dynamical system (or a
-- dependent Moore machine, or simply a dynamical system) is a lens
-- 𝜙: 𝑆y^𝑆 → 𝑝
-- for some 𝑆 ∈ Set and 𝑝 ∈ Poly. The set 𝑆 is called the set of states—with 𝑆y^𝑆
-- called the
-- state system—and the polynomial 𝑝 is called the interface. Positions of the interface are
-- called outputs, and directions of the interface are called inputs.
-- The lens’s on-positions function 𝜙1 : 𝑆 → 𝑝(1) is called the return function, and for
-- each 𝑠 ∈ 𝑆, the lens’s on-directions function 𝜙♯𝑠 : 𝑝[𝜙1(𝑠)] → 𝑆 is called the update
-- function at 𝑠.

-- Ex Example 3.28 from https://topos.site/poly-book.pdf

type Sr = [(Natural, Natural)]

𝜙1 = undefined -- Return

s :: Natural -> Sr
s n = do
  na <- [1 .. n]
  nb <- [1 .. n]
  pure (na, nb)

d :: (Num a1, Num a2, Ord a1) => a1 -> a1 -> [a2]
d n i
  | i == 1 = [0, 1]
  | 1 < i && i < n = [negate 1, 0, 1]
  | otherwise = [negate 1, 0]