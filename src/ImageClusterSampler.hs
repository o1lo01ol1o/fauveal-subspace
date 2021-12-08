{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ImageClusterSampler where

import Control.Monad (forM)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace (trace)
import System.Random.Shuffle (shuffleM)
import Torch

debug :: c -> String -> c
debug = flip trace

for :: Functor t => t a -> (a -> b) -> t b
for = flip fmap

dataSet :: IO [Map Int [Int]]
dataSet = do
  csvData <- BL.readFile "python/munger/data/preprocessed/color_label.csv"
  V.toList <$> case decode NoHeader csvData of
    Left err -> error err
    Right v -> pure . for v $ \(id' :: Double, r, g, b, s :: Double) -> Map.singleton (Prelude.floor id') (fmap Prelude.floor [r, g, b, s])

shuffle :: (Ord k, Semigroup a) => [Map k a] -> IO (Map k a)
shuffle d = do
  Map.unionsWith (<>) <$> shuffleM d

dataTensor :: Map Int [Int] -> Tensor
dataTensor d = asTensor $ fmap (go . fmap (fromIntegral @Int @Float) . snd) (Map.toList d)
  where
    go :: [Float] -> [Float]
    go v
      | length v < 21 * 4 =
        let pad = 21 * 4 - fromIntegral (length v)
         in replicate pad (0 :: Float) <> v
      | otherwise = v