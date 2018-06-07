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
import qualified Data.Text as T
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
import           System.Process.Typed(Process, proc, startProcess, stopProcess)

import Paths_radiod(getLibexecDir)
import System.Radiod.ConfigFile(loadConfigFile)
import System.Radiod.Types

--
-- RIGCTLD CONTROL
--

startRigctld :: FilePath -> Integer -> Maybe Integer -> IO (Process () () ())
startRigctld dev ty port = do
    dir <- getLibexecDir
    let fp   = dir </> "rigctld-wrapper"
        args = ["-m", show ty, "-r", dev] ++ maybe [] (\p -> ["-t", show p]) port
    startProcess (proc fp args)

stopRigctld :: Process stdin stdout stderr -> IO ()
stopRigctld = stopProcess

--
-- ROTCTLD CONTROL
--

startRotctld :: FilePath -> Integer -> Maybe Integer -> IO (Process () () ())
startRotctld dev ty port = do
    dir <- getLibexecDir
    let fp   = dir </> "rotctld-wrapper"
        args = ["-m", show ty, "-r", dev] ++ maybe [] (\p -> ["-t", show p]) port
    startProcess (proc fp args)

stopRotctld :: Process stdin stdout stderr -> IO ()
stopRotctld = stopProcess

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
