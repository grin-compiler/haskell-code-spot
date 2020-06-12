{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module FilterEvents where

import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as LText
import GHC.RTS.Events as GHC
import Web.Scotty


type EventName = LText.Text

-- eventTypeNum from GHC.RTS.Events.Binary would be a nicer solution here.
filterEvents :: [EventName] -> [Event] -> [Event]
filterEvents en = mapMaybe (matchEvent en)

matchEvent :: [EventName] -> Event -> Maybe Event
matchEvent en e = case take 1 $ words $ show $ evSpec e of
  [n] | LText.pack n `elem` en -> Just e
  _                            -> Nothing

-- | Return 'Just eventNames' for the filter to keep. Otherwise Nothing.
eventFilters :: ActionM (Maybe [EventName])
eventFilters
  = fmap
      (\case { [] -> Nothing ; en -> Just en }
      . mapMaybe (\case { ("event-type", v) -> Just v ; _ -> Nothing})
      )
  $ params

