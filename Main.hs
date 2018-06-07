{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent(threadDelay)
import           Control.Concurrent.MVar(MVar, newEmptyMVar, putMVar, tryTakeMVar)
import           Control.Monad(unless, void, when)
import qualified Data.ByteString.Char8 as C8
import           Data.IORef(IORef, atomicModifyIORef, atomicWriteIORef, newIORef, readIORef)
import qualified Data.Map as Map
import           Data.Maybe(mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Directory(setCurrentDirectory , doesFileExist, removeFile)
import           System.Exit(exitFailure)
import           System.FilePath((</>))
import           System.INotify
import           System.IO(hFlush, stdout)
import           System.Posix.Files(setFileCreationMask, stdFileMode)
import           System.Posix.IO
import           System.Posix.Process(createSession, forkProcess, getProcessID)
import           System.Posix.Signals(Handler(..), installHandler, sigCHLD, sigHUP, sigTERM)
import           System.Posix.User(getEffectiveUserID)
import           System.Process(ProcessHandle, spawnProcess, terminateProcess)
import           Text.Read(readMaybe)

import Paths_radiod(getLibexecDir, getSysconfDir)

data Device = Rig Integer (Maybe Integer)
            | Rot Integer (Maybe Integer)
 deriving(Eq, Show)

type DeviceMap = Map.Map T.Text Device

type ProcMap   = Map.Map T.Text ProcessHandle

--
-- CONFIG FILE
--

parseConfigFile :: T.Text -> DeviceMap
parseConfigFile str | strs <- T.lines str =
    Map.fromList $ mapMaybe parseOneLine strs
 where
    parseOneRig :: [T.Text] -> Maybe (T.Text, Device)
    parseOneRig l = readMaybe (T.unpack $ l !! 2) >>= \ty -> do
        let port = if length l > 3 then readMaybe (T.unpack $ l !! 3) else Nothing
        Just (head l, Rig ty port)

    parseOneRotor :: [T.Text] -> Maybe (T.Text, Device)
    parseOneRotor l = readMaybe (T.unpack $ l !! 2) >>= \ty -> do
        let port = if length l > 3 then readMaybe (T.unpack $ l !! 3) else Nothing
        Just (head l, Rot ty port)

    parseOneLine :: T.Text -> Maybe (T.Text, Device)
    parseOneLine input = let
        stripped = T.strip input
     in
        if T.null stripped || "#" `T.isPrefixOf` stripped then Nothing
        else let
            l = T.split (== ',') stripped
         in
            if | length l >= 3 && T.toUpper (l !! 1) == "RIG"   -> parseOneRig l
               | length l >= 3 && T.toUpper (l !! 1) == "ROTOR" -> parseOneRotor l
               | otherwise                                      -> Nothing

loadConfigFile :: IO DeviceMap
loadConfigFile = do
    dir <- getSysconfDir
    c   <- doesFileExist (dir </> "radiod.conf") >>= \case
               True  -> TIO.readFile (dir </> "radiod.conf")
               False -> return ""

    return $ parseConfigFile c

--
-- RIGCTLD CONTROL
--

startRigctld :: FilePath -> Integer -> Maybe Integer -> IO ProcessHandle
startRigctld dev ty port = do
    dir <- getLibexecDir
    let args = ["-m", show ty, "-r", dev] ++ maybe [] (\p -> ["-t", show p]) port
    spawnProcess (dir </> "rigctld-wrapper") args

stopRigctld :: ProcessHandle -> IO ()
stopRigctld = terminateProcess

--
-- ROTCTLD CONTROL
--

startRotctld :: FilePath -> Integer -> Maybe Integer -> IO ProcessHandle
startRotctld dev ty port = do
    dir <- getLibexecDir
    let args = ["-m", show ty, "-r", dev] ++ maybe [] (\p -> ["-t", show p]) port
    spawnProcess (dir </> "rotctld-wrapper") args

stopRotctld :: ProcessHandle -> IO ()
stopRotctld = terminateProcess

--
-- INOTIFY STUFF
--

insert :: Ord k => IORef (Map.Map k a) -> k -> a -> IO ()
insert ref key val =
    atomicModifyIORef ref (\m -> (Map.insert key val m, ()))

delete :: Ord k => IORef (Map.Map k a) -> k -> IO ()
delete ref key =
    atomicModifyIORef ref (\m -> (Map.delete key m, ()))

handler :: IORef ProcMap -> IORef DeviceMap -> Event -> IO ()
handler procRef devRef Created{isDirectory=False, filePath=fp} = do
    devMap <- readIORef devRef
    let fp' = "/dev" </> C8.unpack fp

    case Map.lookup (T.pack fp') devMap of
        Just (Rig ty port) -> do h <- startRigctld fp' ty port
                                 insert procRef (T.pack fp') h
        Just (Rot ty port) -> do h <- startRotctld fp' ty port
                                 insert procRef (T.pack fp') h
        _                  -> return ()

handler procRef devRef Deleted{isDirectory=False, filePath=fp} = do
    devMap <- readIORef devRef
    let fp' = "/dev" </> C8.unpack fp

    case Map.lookup (T.pack fp') devMap of
        Just (Rig _ _) -> do procMap <- readIORef procRef
                             case Map.lookup (T.pack fp') procMap of
                                 Nothing -> return ()
                                 Just h  -> do stopRigctld h
                                               delete procRef (T.pack fp')
        Just (Rot _ _) -> do procMap <- readIORef procRef
                             case Map.lookup (T.pack fp') procMap of
                                 Nothing -> return ()
                                 Just h  -> do stopRotctld h
                                               delete procRef (T.pack fp')
        _              -> return ()

handler _ _ _ = return ()

--
-- DAEMON STUFF
--

reconnectHandles :: IO ()
reconnectHandles = do
    devnull <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
    void $ dupTo devnull stdInput
    closeFd devnull

    fd <- openFd "/dev/null" ReadWrite (Just stdFileMode) defaultFileFlags
    hFlush stdout
    mapM_ (dupTo fd) [stdOutput, stdError]
    closeFd fd

--
-- MAIN PROGRAM
--

loop :: MVar () -> IO ()
loop v = tryTakeMVar v >>= \case
    Just _  -> return ()
    Nothing -> threadDelay 1000000 >> loop v

program :: IO ()
program = do
    devMap  <- loadConfigFile >>= newIORef
    procMap <- newIORef Map.empty
    inotify <- initINotify

    void $ do
        v <- newEmptyMVar

        void $ installHandler sigCHLD Ignore Nothing
        void $ installHandler sigHUP  (Catch $ loadConfigFile >>= atomicWriteIORef devMap) Nothing
        void $ installHandler sigTERM (CatchOnce $ putMVar v ()) Nothing

        void $ addWatch inotify [Create, Delete] "/dev" (handler procMap devMap)
        loop v

main :: IO ()
main = do
    uid <- getEffectiveUserID
    unless (uid == 0) $ do
        putStrLn "This program must be run as root."
        exitFailure

    exists <- doesFileExist "/var/run/radiod.run"
    when exists $ do
        putStrLn "radiod is already running."
        exitFailure

    -- TODO: Close open file descriptors.

    -- TODO: Reset signal handlers to default.

    -- TODO: Reset the signal mask.

    -- Do the double fork thing so we can't get a terminal again.
    -- Also create an independent session in the middle.
    void $ forkProcess $ do
        void $ createSession
        void $ forkProcess $ do
            -- Write a PID file.  There's a short window between here and where we
            -- check to see if the file already exists, but I don't expect this
            -- to be a very popular program so whatever.
            pid <- getProcessID
            writeFile "/var/run/radiod.run" (show pid ++ "\n")

            -- Connect /dev/null to stdin, stdout, and stderr
            reconnectHandles

            -- Reset the umask to 0.
            void $ setFileCreationMask 0

            -- Change the current directory to / so we don't block unmounting.
            setCurrentDirectory "/"

            -- Finally, run the real program.
            program

            -- When 'program' returns, we'll have gotten a SIGTERM (or something), so
            -- it's time to clean up.
            removeFile "/var/run/radiod.run"
