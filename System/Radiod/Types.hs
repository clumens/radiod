module System.Radiod.Types(Device(..),
                           DeviceInfo,
                           DeviceMap,
                           ProcMap)
 where

import qualified Data.Map as Map
import qualified Data.Text as T
import           System.Process.Typed(Process)

data Device = Rig Integer (Maybe Integer) T.Text            -- ^ model number, port, description
            | Rot Integer (Maybe Integer) T.Text            -- ^ model number, port, description
 deriving(Eq, Show)

type DeviceInfo = (String, Integer)

type DeviceMap = Map.Map T.Text Device

type ProcMap   = Map.Map T.Text (Process () () ())
