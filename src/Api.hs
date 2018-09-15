{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Api where

import           Config                   (HasNetworkConfig, networkConfig,
                                           port)
import           Control.Lens             (view)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

data Item = Item
  { category :: String
  , link     :: String
  } deriving (Show, Generic)

instance ToJSON Item

instance FromJSON Item

type ItemApi = "item" :> Get '[ JSON] [Item]

itemApi :: Proxy ItemApi
itemApi = Proxy

getItems :: (MonadIO m, MonadReader r m, HasNetworkConfig r) => m [Item]
getItems =
  return [Item "foo" "bar"]

mkApp :: Application
mkApp = serve itemApi getItems

run :: (MonadIO m, MonadReader r m, HasNetworkConfig r) => m ()
run = do
  serverPort <- view (networkConfig . port)
  let settings =
        setPort serverPort $
        setBeforeMainLoop (liftIO $ hPutStrLn stderr ("listening on port " ++ show serverPort)) defaultSettings
  liftIO $ runSettings settings mkApp

printM :: (MonadIO m, Show a) => a -> m ()
printM a = liftIO $ print a
