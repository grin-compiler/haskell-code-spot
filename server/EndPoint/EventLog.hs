{-# LANGUAGE OverloadedStrings, LambdaCase, RecordWildCards #-}
module EndPoint.EventLog (endpoints) where

import Web.Scotty
import Control.Monad.IO.Class
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?), object, (.=))
import Data.List.Extra
import EventlogJSON
import FilterEvents

import qualified Data.Text.Lazy as LText
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as Base64
import qualified GHC.RTS.Events as GHC


data EventLog = EventLog
  { eventLogPath    :: FilePath
  , eventLogOffset  :: Maybe Int
  , eventLogIndex   :: Maybe Int
  , eventLogFilters :: Maybe [LText.Text]
  } deriving (Show)

instance FromJSON EventLog where
    parseJSON = withObject "/ghc_stgapp parameters" $ \v -> EventLog
        <$> v .: "path"
        <*> v .:? "offset"
        <*> v .:? "idx"
        <*> (fmap (nonEmpty =<<) (v .:? "event-type")) -- Empty event-type list is converted to Nothing

endpoints = post "/eventlog" $ do
  d@EventLog{..} <- jsonData
  liftIO $ putStrLn $ "/eventlog got " ++ show d
  liftIO (GHC.readEventLogFromFile eventLogPath) >>= \case
    Left err -> do
      liftIO $ putStrLn "eventlog error"
      raise $ LText.pack err
    Right all@(GHC.EventLog h (GHC.Data evs)) -> do
      let evlog = GHC.EventLog h $ GHC.Data
                $ maybe id filterEvents eventLogFilters -- Keep the events of such kind
                $ maybe id take eventLogIndex           -- Take this number of events
                $ maybe id drop eventLogOffset          -- Skip the beginning
                $ evs
      liftIO $ do
        putStrLn "eventlog success"
      json evlog
