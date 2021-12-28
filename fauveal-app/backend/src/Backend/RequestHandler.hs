{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Backend.RequestHandler where

import Backend.Transaction (Transaction, runQuery)
import Common.App
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Nat (Nat (Z))
import Data.Proxy
import Data.Type.Nat
import Data.Vec.Lazy (Vec ((:::)), (!))
import GHC.Generics (Generic)
import Rhyolite.Api (ApiRequest (..))
import Rhyolite.Backend.App (RequestHandler (..))
import Servant.API
import Servant.Client (ClientM, client)

data Result a = Result
  { msg :: String,
    result :: a
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type InferAPI =
  "inference" :> Capture "p0" Float
    :> Capture "p1" Float
    :> Capture "p2" Float
    :> Capture "p3" Float
    :> Capture "p4" Float
    :> Capture "p5" Float
    :> Capture "p6" Float
    :> Capture "p7" Float
    :> Get '[JSON] (Result [Float])

torchApi :: Proxy InferAPI
torchApi = Proxy

getVAE8 ::
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  ClientM (Result [Float])
getVAE8 = client torchApi

bundleVAE8 ::
  Four (Two Float) -> ClientM (Result [Float])
bundleVAE8 q =
  let [x0, x1, x2, x3, x4, x5, x6, x7] = foldMap (foldMap (: [])) q
   in getVAE8 x0 x1 x2 x3 x4 x5 x6 x7

foldIntoFour ::
  [Four a] ->
  [a] ->
  [ Four
      a
  ]
foldIntoFour acc [] = acc
foldIntoFour acc (a : b : c : d : xs) = foldIntoFour (Four a b c d : acc) xs
foldIntoFour _ _ = error "result does not contain mod 4 elements"

handleResult :: Result [Four Float] -> IO (Maybe [Four Float])
handleResult (Result msg' result')
  | null result' = do
    putStrLn msg'
    pure Nothing
  | otherwise = pure $ Just result'

handleVAE8 :: Result [Float] -> IO (Maybe [Four Float])
handleVAE8 (Result msg' result')
  | length result' `mod` 4 == 0 = pure . Just . foldIntoFour mempty $ result'
  | null result' = do
    putStrLn msg'
    pure Nothing
  | otherwise = do
    putStrLn "handleVAE8: No results"
    pure Nothing

requestHandler :: (forall x. Transaction mode x -> m x) -> RequestHandler (ApiRequest () PublicRequest PrivateRequest) m
requestHandler runTransaction =
  RequestHandler $
    runTransaction . \case
      ApiRequest_Public (PublicRequest_VAE8 q) -> do
        r <- runQuery $ bundleVAE8 q
        liftIO $ handleVAE8 r
      ApiRequest_Private _key r -> case r of
        PrivateRequest -> return ()
