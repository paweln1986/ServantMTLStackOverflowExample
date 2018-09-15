module Main where

import           Config               (AppConfig (..), NetworkConfig (..))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Lib
import           Types

config = AppConfig $ NetworkConfig 8081 "localhost"

main :: IO ()
main = do
  res <- runApplication config someFunc
  case res of
    Left (AppDbError a) -> print $ show a
    Right r             -> print "done"
  return ()
  where
    runApplication config = runExceptT . flip runReaderT config . runApp
