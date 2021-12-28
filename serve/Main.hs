{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Autoencoder
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
import Torch
import Torch.Serialize

data Result = Result
  { msg :: String,
    result :: [Float]
  }
  deriving (Show, Generic)

instance ToJSON Result

instance FromJSON Result

type InferAPI =
  "inference"
    :> Capture "p0" Float
    :> Capture "p1" Float
    :> Capture "p2" Float
    :> Capture "p3" Float
    :> Capture "p4" Float
    :> Capture "p5" Float
    :> Capture "p6" Float
    :> Capture "p7" Float
    :> Get '[JSON] Result

torchApi :: Proxy InferAPI
torchApi = Proxy

wrapModel :: (MonadIO f) => VAEState -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> f Result
wrapModel trained x0 x2 x3 x4 x5 x6 x7 x8 =
  let xs' = [x0, x2, x3, x4, x5, x6, x7, x8]
   in do
        liftIO $ print xs'
        liftIO $
          pure $ Result "infer" $ asValue @[Float] (projectLatent trained (asTensor xs'))

main :: IO ()
main = do
  init <- makeModel dataDim hDim zDim
  trained' <- loadParams init "/Users/timpierson/arity/fauveal-subspace/data/models/vaeState.pt"

  putStrLn $ "\nRunning server on port " ++ show port
  putStrLn $ "\nTry a model inference:\n\nhttp://localhost:" ++ show port ++ "/inference/1.0/2.0/3.0/"

  run port (serve torchApi (wrapModel trained'))
  where
    dataDim = 4 * 21
    hDim = 16 * 21 -- hidden layer dimensions
    zDim = 16 -- latent space (z) dimensions
    port = 8081
