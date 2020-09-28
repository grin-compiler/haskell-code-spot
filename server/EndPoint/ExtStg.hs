{-# LANGUAGE OverloadedStrings, LambdaCase, RecordWildCards, DeriveGeneric, DeriveAnyClass #-}
module EndPoint.ExtStg where

import GHC.Generics

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (FromJSON(..), withObject, (.:), object, (.=), Value(..))
import Data.Aeson.Types (Parser)
import Data.Text (pack)
import Web.Scotty

import qualified Data.List as List

import Stg.IO
import Stg.Program

import qualified Data.ByteString.Char8 as BS8

data ParamGetSourceCode
  = ParamGetSourceCode
  { modpakPath :: String
  }
  deriving (Generic, Show, FromJSON)

getSourceCode = post "/ext-stg/get-source-code" $ do
    ParamGetSourceCode{..} <- jsonData
    ok <- liftIO $ doesModpakEntryExist modpakPath modpakHaskellSourcePath
    src <- if not ok
            then pure ""
            else liftIO $ readModpakS modpakPath modpakHaskellSourcePath id
    json $ object ["sourceCode" .= BS8.unpack src, "hasSource" .= ok]

-------------------

data ParamGetModpakContent
  = ParamGetModpakContent
  { gmpcModpakPath  :: String
  , gmpcFiles       :: [String]
  }
  deriving (Generic, Show)

instance FromJSON ParamGetModpakContent where
    parseJSON = withObject "/ext-stg/get-modpak-content" $ \v -> ParamGetModpakContent
        <$> v .: "modpakPath"
        <*> v .: "files"

getGetModpakContent = post "/ext-stg/get-modpak-content" $ do
    ParamGetModpakContent{..} <- jsonData
    content <- forM gmpcFiles $ \fname -> liftIO $ do
      ok <- doesModpakEntryExist gmpcModpakPath fname
      case ok of
        False -> pure [pack fname .= Null]
        True  -> do
          v <- readModpakS gmpcModpakPath fname id
          pure [pack fname .= BS8.unpack v]
    json . object $ concat content

-------------------

data ParamGetModuleMapping
  = ParamGetModuleMapping
  { gmmGhcStgAppPath  :: String
  }

instance FromJSON ParamGetModuleMapping where
    parseJSON = withObject "/ext-stg/get-module-mapping" $ \v -> ParamGetModuleMapping
        <$> v .: "ghcStgAppPath"

getModuleMapping = post "/ext-stg/get-module-mapping" $ do
    ParamGetModuleMapping{..} <- jsonData
    m <- liftIO $ getAppModuleMapping gmmGhcStgAppPath
    json $ object [pack modModuleName .= object ["modpakPath" .= modModpakPath, "packageName" .= modPackageName] | StgModuleInfo{..} <- m]

-------------------

endpoints = do
  getSourceCode
  getModuleMapping
  getGetModpakContent
