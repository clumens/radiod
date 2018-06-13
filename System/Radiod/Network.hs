{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module System.Radiod.Network(Request(..),
                             Response(..),
                             Message(..),
                             radiodPort)
 where

import Data.Serialize(Serialize)
import GHC.Generics(Generic)

import System.Radiod.Types(DeviceInfo)

data Request = ReqAllConnectedRadios
             | ReqAllConnectedRotors
 deriving (Eq, Generic, Serialize, Show)

data Response = RespError
              | RespOK
              | RespAllConnectedRadios  [DeviceInfo]
              | RespAllConnectedRotors  [DeviceInfo]
              | RespRadioConnected      DeviceInfo
              | RespRadioDisconnected   DeviceInfo
              | RespRotorConnected      DeviceInfo
              | RespRotorDisconnected   DeviceInfo
              | RespShuttingDown
 deriving (Eq, Generic, Serialize, Show)

data Message = MsgRequest Request
             | MsgResponse Response
 deriving (Eq, Generic, Serialize, Show)

radiodPort :: Int
radiodPort = 51001
