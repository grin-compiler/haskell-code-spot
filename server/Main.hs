{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where

import Web.Scotty

import Data.Maybe (mapMaybe)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as LText
import Control.Monad.IO.Class

import Network.WebSockets as WS
import Network.Wai.Handler.WebSockets as WS
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger as Warp
import Network.Wai.Middleware.Cors

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as Base64
import qualified GHC.RTS.Events as GHC

import Network.HTTP.Types.Status (created201, internalServerError500, notFound404)

import EventlogJSON
import FilterEvents
import qualified EndPoint.SourceView  as SourceView
import qualified EndPoint.EventLog    as EventLog
import qualified EndPoint.FileView    as FileView

port = 3000

main :: IO ()
main = do
  putStrLn $ "localhost:" ++ show port
  app <- httpApp
  Warp.run port
    $ Warp.logStdoutDev
    $ WS.websocketsOr
        WS.defaultConnectionOptions
        wsApp
        app

httpApp = scottyApp $ do
  middleware simpleCors
  EventLog.endpoints
  SourceView.endpoints
  FileView.endpoints
  notFound notFoundA

notFoundA :: ActionM ()
notFoundA = do
  status notFound404
  json ()

wsApp :: WS.ServerApp
wsApp pending_conn = do
    conn <- acceptRequest pending_conn
    sendTextData conn ("Hello, client!" :: LText.Text)
