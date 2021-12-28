{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Backend.Transaction where

import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (..))
import Servant.Client (ClientEnv, ClientM, runClientM)

newtype Transaction mode a = Transaction {unTransaction :: ReaderT ClientEnv IO a}
  deriving (Functor, Applicative, Monad, MonadThrow, MonadIO)

runTransaction :: MonadIO m => ClientEnv -> Transaction mode a -> m a
runTransaction env (Transaction (ReaderT act)) = liftIO $ act env

runQuery :: ClientM a -> Transaction mode a
runQuery act =
  Transaction $
    ReaderT
      ( \env -> do
          r <- runClientM act env
          case r of
            Left err -> throwM err
            Right a -> pure a
      )
