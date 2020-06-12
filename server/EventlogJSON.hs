{-# LANGUAGE TemplateHaskell #-}
module EventlogJSON where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.TH

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64

import GHC.RTS.Events as GHC


instance FromJSON BS.ByteString where
  parseJSON a = parseJSON a >>= either fail pure . Base64.decode
instance ToJSON BS.ByteString where
  toJSON = toJSON . Base64.encode
  toEncoding = toEncoding . Base64.encode

$(deriveJSON defaultOptions ''HeapProfFlags)
$(deriveJSON defaultOptions ''KernelThreadId)
$(deriveJSON defaultOptions ''HeapProfBreakdown)
$(deriveJSON defaultOptions ''ThreadStopStatus)
$(deriveJSON defaultOptions ''CapsetType)
$(deriveJSON defaultOptions ''MessageTag)
$(deriveJSON defaultOptions ''EventType)
$(deriveJSON defaultOptions ''EventInfo)
$(deriveJSON defaultOptions ''Event)
$(deriveJSON defaultOptions ''Data)
$(deriveJSON defaultOptions ''Header)
$(deriveJSON defaultOptions ''EventLog)
