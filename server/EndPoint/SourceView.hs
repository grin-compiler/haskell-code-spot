{-# LANGUAGE OverloadedStrings, LambdaCase, RecordWildCards #-}
module EndPoint.SourceView where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON(..), withObject, (.:), object, (.=))
import Web.Scotty

data ParamGHCStgApp
  = ParamGHCStgApp
  { ghcStgAppPath :: String
  }
  deriving Show

instance FromJSON ParamGHCStgApp where
    parseJSON = withObject "/ghc_stgapp parameters" $ \v -> ParamGHCStgApp
        <$> v .: "path"

endpoints = do
  post "/ghc_stgapp" $ do
    ParamGHCStgApp{..} <- jsonData
    liftIO $ putStrLn $ "/ghc_stgapp: path is " ++ show ghcStgAppPath
    json $ object ["name" .= ("hello" :: String), "age" .= ("35" :: String)]
