{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Data.Monoid (mconcat)
import Data.Text

import Network.WebSockets as WS
import Network.Wai.Handler.WebSockets as WS
import Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  app <- httpApp
  Warp.run 3000 $
    WS.websocketsOr
      WS.defaultConnectionOptions
      wsApp
      app

httpApp = scottyApp $ do
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

wsApp :: WS.ServerApp
wsApp pending_conn = do
    conn <- acceptRequest pending_conn
    sendTextData conn ("Hello, client!" :: Text)
