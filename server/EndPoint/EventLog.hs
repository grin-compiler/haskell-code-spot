{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module EndPoint.EventLog (endpoints) where

import Web.Scotty

import qualified Data.Text.Lazy as LText
import Control.Monad.IO.Class

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as Base64
import qualified GHC.RTS.Events as GHC

import EventlogJSON
import FilterEvents


endpoints = get "/eventlog/:path" $ do
  eventlogPath <- BS8.unpack . Base64.decodeLenient <$> param "path"
  (mOffset, mIdx) <- range
  mEventFilters <- eventFilters
  liftIO $ putStrLn $ "got evlog request for " ++ show (eventlogPath, mOffset, mIdx)
  liftIO (GHC.readEventLogFromFile eventlogPath) >>= \case
    Left err -> do
      liftIO $ putStrLn "eventlog error"
      raise $ LText.pack err
    Right all@(GHC.EventLog h (GHC.Data evs)) -> do
      let evlog = GHC.EventLog h $ GHC.Data
                $ maybe id filterEvents mEventFilters -- Keep the events of such kind
                $ maybe id take mIdx    -- Take this number of events
                $ maybe id drop mOffset -- Skip the beginning
                $ evs
      liftIO $ do
        putStrLn "eventlog success"
      json evlog

range :: ActionM (Maybe Int, Maybe Int)
range = do
  ps <- params
  pure (read . LText.unpack <$> lookup "offset" ps, read . LText.unpack <$> lookup "idx" ps)
