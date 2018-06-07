module System.Radiod.Types(Device(..),
                           DeviceMap,
                           ProcMap)
 where

import qualified Data.Map as Map
import qualified Data.Text as T
import           System.Process.Typed(Process)

data Device = Rig Integer (Maybe Integer)
            | Rot Integer (Maybe Integer)
 deriving(Eq, Show)

type DeviceMap = Map.Map T.Text Device

type ProcMap   = Map.Map T.Text (Process () () ())
