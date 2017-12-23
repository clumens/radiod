{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import           Control.Concurrent(threadDelay)
import           Control.Monad(forM_, forever, unless, void)
import           Data.Char(isSpace, toUpper)
import           Data.IORef(IORef, atomicModifyIORef, atomicWriteIORef, newIORef, readIORef)
import           Data.List(dropWhileEnd)
import qualified Data.Map as Map
import           Data.Maybe(isNothing, mapMaybe)
import           System.Directory(setCurrentDirectory , doesFileExist)
import           System.Exit(exitFailure)
import           System.FilePath((</>))
import           System.INotify
import           System.IO(IOMode(..), hFlush, stdin, stdout, withFile)
import           System.Posix.Files(setFileCreationMask, stdFileMode)
import           System.Posix.IO
import           System.Posix.Process(createSession, forkProcess)
import           System.Posix.Signals(Handler(..), installHandler, sigCHLD, sigHUP)
import           System.Posix.User(getEffectiveUserID)
import           System.Process(ProcessHandle, spawnProcess, terminateProcess)

import Paths_radiod(getLibexecDir, getSysconfDir)

data Device = Rig Integer (Maybe Integer)
            | Rot Integer (Maybe Integer)
 deriving(Eq, Show)

type DeviceMap = Map.Map String Device

type ProcMap   = Map.Map String ProcessHandle

--
-- CONFIG FILE
--

-- TODO:  This should support blank lines and comments.
parseConfigFile :: String -> DeviceMap
parseConfigFile str | strs <- lines str =
    Map.fromList $ mapMaybe parseOneLine strs
 where
    stringToInt :: String -> Maybe Integer
    stringToInt s = case reads s of
        [(val, "")] -> Just val
        _           -> Nothing

    uppercase :: String -> String
    uppercase = map toUpper

    parseOneRig :: [String] -> Maybe (String, Device)
    parseOneRig l = stringToInt (l !! 2) >>= \ty -> do
        let port = stringToInt (l !! 3)
        Just (head l, Rig ty port)

    parseOneRotor :: [String] -> Maybe (String, Device)
    parseOneRotor l = stringToInt (l !! 2) >>= \ty -> do
        let port = stringToInt (l !! 3)
        Just (head l, Rot ty port)

    parseOneLine :: String -> Maybe (String, Device)
    parseOneLine l | l <- words l =
        if | length l == 4 && uppercase (l !! 1) == "RIG"   -> parseOneRig l
           | length l == 4 && uppercase (l !! 1) == "ROTOR" -> parseOneRotor l
           | otherwise -> Nothing

loadConfigFile :: IO DeviceMap
loadConfigFile = do
    dir <- getSysconfDir
    c   <- doesFileExist (dir </> "radiod.conf") >>= \case
               True  -> readFile (dir </> "radiod.conf")
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

handler :: IORef ProcMap -> IORef DeviceMap -> Event -> IO ()
handler procRef devRef Created{isDirectory=False, filePath=fp} = do
    devMap <- readIORef devRef
    let fp' = "/dev" </> fp

    case Map.lookup fp' devMap of
        Just (Rig ty port) -> do h <- startRigctld fp' ty port
                                 atomicModifyIORef procRef (\m -> (Map.insert fp' h m, ()))
        Just (Rot ty port) -> do h <- startRotctld fp' ty port
                                 atomicModifyIORef procRef (\m -> (Map.insert fp' h m, ()))
        _                  -> return ()

handler procRef devRef Deleted{isDirectory=False, filePath=fp} = do
    devMap <- readIORef devRef
    let fp' = "/dev" </> fp

    case Map.lookup fp' devMap of
        Just (Rig ty port) -> do procMap <- readIORef procRef
                                 case Map.lookup fp' procMap of
                                     Nothing -> return ()
                                     Just h  -> do stopRigctld h
                                                   atomicModifyIORef procRef (\m -> (Map.delete fp' m, ()))
        Just (Rot ty port) -> do procMap <- readIORef procRef
                                 case Map.lookup fp' procMap of
                                     Nothing -> return ()
                                     Just h  -> do stopRotctld h
                                                   atomicModifyIORef procRef (\m -> (Map.delete fp' m, ()))
        _                  -> return ()

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

program :: IO ()
program = do
    devMap  <- loadConfigFile >>= newIORef
    procMap <- newIORef Map.empty
    inotify <- initINotify

    void $ do
        installHandler sigCHLD Ignore Nothing
        installHandler sigHUP  (Catch $ loadConfigFile >>= atomicWriteIORef devMap) Nothing

        addWatch inotify [Create, Delete] "/dev" (handler procMap devMap)
        forever $
            threadDelay 1000000

main :: IO ()
main = do
    uid <- getEffectiveUserID
    unless (uid == 0) $ do
        putStrLn "This program must be run as root."
        exitFailure

    -- TODO: Close open file descriptors.

    -- TODO: Reset signal handlers to default.

    -- TODO: Reset the signal mask.

    -- Do the double fork thing so we can't get a terminal again.
    -- Also create an independent session in the middle.
    void $ forkProcess $ do
        createSession
        void $ forkProcess $ do
            -- Connect /dev/null to stdin, stdout, and stderr
            reconnectHandles

            -- Reset the umask to 0.
            void $ setFileCreationMask 0

            -- Change the current directory to / so we don't block unmounting.
            setCurrentDirectory "/"

            -- Finally, run the real program.
            program
