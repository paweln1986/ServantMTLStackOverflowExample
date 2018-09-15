{-# LANGUAGE TemplateHaskell #-}

module Config where

import           Control.Lens (makeClassy)

data NetworkConfig = NetworkConfig
  { _port :: Int
  , _host :: String
  }

makeClassy ''NetworkConfig

newtype AppConfig = AppConfig
  { _appNetConfig :: NetworkConfig
  }

makeClassy ''AppConfig

instance HasNetworkConfig AppConfig where
  networkConfig = appNetConfig
