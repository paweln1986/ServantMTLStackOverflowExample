module Lib where

import           Api                  (run)
import           Config               (HasNetworkConfig, host, networkConfig,
                                       port)
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Types                (AppT, AsNetworkError)

someFunc :: (MonadIO m) => AppT m ()
someFunc = do
  test
  run

printM :: (MonadIO m) => String -> m ()
printM = liftIO . print

test :: (MonadIO m, MonadError e m, AsNetworkError e, MonadReader r m, HasNetworkConfig r) => m ()
test = do
  printM "asdasd"
  aaa <- view (networkConfig . port)
  bbb <- view (networkConfig . host)
  printM $ "port" ++ show aaa
  printM $ "host" ++ bbb
