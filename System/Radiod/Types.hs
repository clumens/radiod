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

-- | Basic information about a Device that's useful to clients.  For now, this
-- is just the description and the port number.  If the port number is Nothing,
-- it's simply using the default port.  In this case it's likely the only one
-- of its type connected to the system.
type DeviceInfo = (String, Maybe Integer)

-- | A mapping from a device node to a 'Device' structure.  This mapping is built
-- up by reading the config file and does not change as devices come and go.  It
-- can, however, be updated by SIGHUP which will cause the config file to be
-- reloaded.
type DeviceMap = Map.Map T.Text Device

-- | A mapping from a device node to a running process managing that device node.
-- This mapping changes while radiod is running and devices come and go.
type ProcMap   = Map.Map T.Text (Process () () ())
