{-# LANGUAGE OverloadedStrings, LambdaCase #-}

import Web.Scotty

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

  get "/eventlog/:path" $ do
    eventlogPath <- BS8.unpack . Base64.decodeLenient <$> param "path"
    (mOffset, mIdx) <- range
    liftIO $ putStrLn $ "got evlog request for " ++ show (eventlogPath, mOffset, mIdx)
    liftIO (GHC.readEventLogFromFile eventlogPath) >>= \case
      Left err  -> do
        liftIO $ putStrLn "eventlog error"
        raise $ LText.pack err
      Right all@(GHC.EventLog h (GHC.Data evs)) -> do
        let evlog = GHC.EventLog h $ GHC.Data $ maybe id take mIdx $ maybe id drop mOffset $ evs
        liftIO $ do
          --Aeson.encodeFile (eventlogPath ++ ".json") all
          --Aeson.encodeFile (eventlogPath ++ ".small.json") evlog
          putStrLn "eventlog success"
        json evlog

  notFound notFoundA

range :: ActionM (Maybe Int, Maybe Int)
range = do
  ps <- params
  pure (read . LText.unpack <$> lookup "offset" ps, read . LText.unpack <$> lookup "idx" ps)

notFoundA :: ActionM ()
notFoundA = do
  status notFound404
  json ()

{-
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
-}

wsApp :: WS.ServerApp
wsApp pending_conn = do
    conn <- acceptRequest pending_conn
    sendTextData conn ("Hello, client!" :: LText.Text)
