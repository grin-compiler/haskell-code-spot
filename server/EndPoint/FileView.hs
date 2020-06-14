{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module EndPoint.FileView where

import Web.Scotty
import Control.Monad.IO.Class
import Data.List.Extra
import Data.Aeson (ToJSON(..), FromJSON(..), withObject, (.:), (.:?), object, (.=))

import qualified Data.Text.Lazy as LText
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as Base64
import qualified GHC.RTS.Events as GHC


data FileView = FileView
  { fileViewPath :: String
  } deriving Show

data FileViewContent = FileViewContent
  { fileViewContent :: LText.Text
  }

instance FromJSON FileView where
    parseJSON = withObject "/ghc_stgapp parameters" $ \v -> FileView
        <$> v .: "path"

instance ToJSON FileViewContent where
    toJSON (FileViewContent content) = object
      [ "content" .= content
      ]

endpoints = do
  post "/fileview" $ do
    d@FileView{..} <- jsonData
    liftIO $ print $ "FileView got: " ++ show d
    content <- liftIO $ readFile fileViewPath
    json $ FileViewContent $ LText.pack content
