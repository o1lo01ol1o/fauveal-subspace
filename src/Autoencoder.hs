{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}

module Autoencoder where

import Control.Monad (when)
import Data.IORef
import Data.List (foldl', intersperse, scanl')
import GHC.Generics
import ImageClusterSampler (dataSet, dataTensor, shuffle)
import Torch
  ( Dim (..),
    GD (GD),
    Linear,
    LinearSpec (LinearSpec),
    Optimizer (runStep),
    Parameterized,
    Randomizable (..),
    Tensor,
    Tri (Upper),
    cat,
    cholesky,
    divScalar,
    exp,
    foldLoop,
    linear,
    matmul,
    mean,
    mseLoss,
    pow,
    randnIO',
    randnLikeIO,
    relu,
    shape,
    slice,
    squeezeAll,
    stack,
    sumAll,
  )
import Prelude hiding (exp)

-- Model Specification

data VAESpec = VAESpec
  { encoderSpec :: [LinearSpec],
    muSpec :: LinearSpec,
    logvarSpec :: LinearSpec,
    decoderSpec :: [LinearSpec],
    nonlinearitySpec :: Tensor -> Tensor
  }
  deriving (Generic)

-- Model State

data VAEState = VAEState
  { encoderState :: [Linear],
    muFC :: Linear,
    logvarFC :: Linear,
    decoderState :: [Linear],
    nonlinearity :: Tensor -> Tensor
  }
  deriving (Generic)

instance Randomizable VAESpec VAEState where
  sample VAESpec {..} = do
    encoderState <- mapM sample encoderSpec
    muFC <- sample muSpec
    logvarFC <- sample logvarSpec
    decoderState <- mapM sample decoderSpec
    let nonlinearity = nonlinearitySpec
    pure $ VAEState {..}

instance Parameterized VAEState

-- Output including latent mu and logvar used for VAE loss

data ModelOutput = ModelOutput
  { recon :: Tensor,
    mu :: Tensor,
    logvar :: Tensor
  }
  deriving (Show)

-- Recon Error + KL Divergence VAE Loss
vaeLoss :: Tensor -> Tensor -> Tensor -> Tensor -> Tensor
vaeLoss recon_x x mu logvar = reconLoss + kld
  where
    -- reconLoss = binary_cross_entropy_loss recon_x x undefined ReduceSum
    reconLoss = mseLoss x recon_x
    kld = -0.5 * sumAll (1 + logvar - pow (2 :: Int) mu - exp logvar)

-- | End-to-end function for VAE model
model :: VAEState -> Tensor -> IO ModelOutput
model VAEState {..} input = do
  let encoded = mlp encoderState nonlinearity input
      mu = linear muFC encoded
      logvar = linear logvarFC encoded
  z <- reparamaterize mu logvar
  let output = mlp decoderState nonlinearity z
  pure $ ModelOutput output mu logvar

-- | MLP helper function for model used by both encoder & decoder
mlp :: [Linear] -> (Tensor -> Tensor) -> Tensor -> Tensor
mlp mlpState nonlin input = foldl' revApply input layerFunctionsList
  where
    layerFunctionsList = intersperse nonlin (map linear mlpState)
    revApply x f = f x

-- | Reparamaterization trick to sample from latent space while allowing differentiation
reparamaterize :: Tensor -> Tensor -> IO Tensor
reparamaterize mu logvar = do
  eps <- randnLikeIO mu
  pure $ mu + eps * exp (0.5 * logvar)

-- | Multivariate 0-mean normal via cholesky decomposition
mvnCholesky :: Tensor -> Int -> Int -> IO Tensor
mvnCholesky cov n axisDim = do
  samples <- randnIO' [axisDim, n]
  pure $ matmul l samples
  where
    l = cholesky Upper cov

-- | Construct and initialize model parameter state
makeModel :: Int -> Int -> Int -> IO VAEState
makeModel dataDim hDim zDim =
  sample
    VAESpec
      { encoderSpec = [LinearSpec dataDim hDim],
        muSpec = LinearSpec hDim zDim,
        logvarSpec = LinearSpec hDim zDim,
        decoderSpec = [LinearSpec zDim hDim, LinearSpec hDim dataDim],
        nonlinearitySpec = relu
      }

trainLoop :: IO ()
trainLoop = do
  losses <- newIORef []
  dat' <- dataSet
  init <- makeModel dataDim hDim zDim
  trained <- foldLoop init numIters $ \vaeState i -> do
    dat <- dataTensor <$> ImageClusterSampler.shuffle dat'
    let startIndex = mod (batchSize * i) nSamples
        endIndex = Prelude.min (startIndex + batchSize) nSamples
        input = slice 0 startIndex endIndex 1 dat -- size should be [batchSize, dataDim]
    output <- model vaeState (divScalar (255 :: Float) input)
    -- print output
    let (reconX, muVal, logvarVal) = (squeezeAll $ recon output, mu output, logvar output)
        loss = vaeLoss reconX input muVal logvarVal
    losses' <- readIORef losses
    writeIORef losses (loss : losses')
    when (i `mod` 100 == 0) $
      print (mean $ stack (Dim 0) losses')
    (new_flat_parameters, _) <- runStep vaeState optimizer loss 1e-8
    pure new_flat_parameters
  putStrLn "Done"
  where
    -- model parameters
    dataDim = 4 * 21
    hDim = 16 * 21 -- hidden layer dimensions
    zDim = 16 -- latent space (z) dimensions
    -- optimization parameters
    optimizer = GD
    nSamples = 937
    batchSize = 256 -- TODO - crashes for case where any batch is of size n=1
    numIters = 60000