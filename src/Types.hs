{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types where

import           Config                 (AppConfig)
import           Control.Lens
import           Control.Monad.Except   (ExceptT, MonadError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT)
import Servant (ServantErr)

data DbError
  = InvalidConnection
  | Lost
  deriving (Show)

data AppError
  = AppDbError { _dbError :: DbError }
  | AppNetError { _netError :: NetworkError }
  deriving (Show)

data NetworkError
  = Timeout Int
  | ServerOnFire
  deriving (Show)

makeClassyPrisms ''DbError

makeClassyPrisms ''AppError

makeClassyPrisms ''NetworkError

instance AsDbError AppError where
  _DbError = _AppError . _AppDbError

instance AsNetworkError AppError where
  _NetworkError = _AppError . _AppNetError

newtype AppT m a = AppT
  { runApp :: ReaderT AppConfig (ExceptT ServantErr m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig, MonadError ServantErr)
