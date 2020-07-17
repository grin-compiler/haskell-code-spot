{-# LANGUAGE OverloadedStrings, LambdaCase, RecordWildCards, DeriveGeneric, DeriveAnyClass #-}
module EndPoint.ExtStg where

import GHC.Generics

import Control.Monad.IO.Class
import Data.Aeson (FromJSON(..), withObject, (.:), object, (.=))
import Data.Aeson.Types (Parser)
import Data.Text (pack)
import Web.Scotty

import qualified Data.List as List

import Stg.IO
import Stg.Program

import qualified Data.ByteString.Char8 as BS8

data ParamGetSourceCode
  = ParamGetSourceCode
  { stgbinPath :: String
  }
  deriving (Generic, Show, FromJSON)

getSourceCode = post "/ext-stg/get-source-code" $ do
    ParamGetSourceCode{..} <- jsonData
    sourceCode <- liftIO $ do
      (_phase, _unitId, _moduleName, _stubs, _hasForeignExported, _deps, src) <- readStgbinSourceCode stgbinPath
      pure src
    case sourceCode of
      Nothing   -> json $ object ["sourceCode" .= ("" :: String), "hasSource" .= False]
      Just src  -> json $ object ["sourceCode" .= BS8.unpack src, "hasSource" .= True]

-------------------

data GetModuleMapping
  = GetModuleMapping
  { gmmGhcStgAppPath  :: String
  }

instance FromJSON GetModuleMapping where
    parseJSON = withObject "/ext-stg/get-module-mapping" $ \v -> GetModuleMapping
        <$> v .: "ghcStgAppPath"

getModuleMapping = post "/ext-stg/get-module-mapping" $ do
    GetModuleMapping{..} <- jsonData
    m <- liftIO $ getAppModuleMapping gmmGhcStgAppPath
    json $ object [pack modModuleName .= object ["stgbinPath" .= modStgbinPath, "packageName" .= modPackageName] | StgModuleInfo{..} <- m]

-------------------

endpoints = do
  getSourceCode
  getModuleMapping
