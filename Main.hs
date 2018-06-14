{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Concurrent.Async(async, cancel, concurrently, waitCatch)
import           Control.Concurrent.STM
import qualified Control.Exception.Safe as CES
import           Control.Monad(forever, forM_, unless, void, when)
import qualified Data.ByteString.Char8 as C8
import           Data.IORef(IORef, atomicModifyIORef, atomicWriteIORef, newIORef, readIORef)
import           Data.Int(Int64)
import qualified Data.Map as Map
import           Data.Serialize(decode, encode)
import qualified Data.Text as T
import           Network(Socket)
import           Network.Socket(AddrInfo(..), AddrInfoFlag(..), SocketType(..), accept, bind, close, defaultHints, defaultProtocol, getAddrInfo, listen, socket)
import           Network.Socket.ByteString(recv, sendAll)
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
import System.Radiod.Network
import System.Radiod.Types

--
-- NETWORKING
--

type ClientId = Int64

data Client = Client { clientId :: ClientId,
                       clientSocket :: Socket,
                       clientSendChan :: TChan Message }

data Server = Server { serverClients :: TVar (Map.Map ClientId Client),
                       serverDeviceMap :: IORef DeviceMap,
                       serverProcMap :: IORef ProcMap }

-- Send a message to all connected clients.
broadcast :: Server -> Message -> IO ()
broadcast Server{..} msg = do
    clients <- readTVarIO serverClients
    forM_ (Map.elems clients) $ \Client{..} ->
        atomically $ writeTChan clientSendChan msg

deleteClient :: Server -> Client -> STM ()
deleteClient Server{..} Client{..} =
    modifyTVar' serverClients $ Map.delete clientId

-- Do whatever the request wants, turning it into a Response message (or an error).
-- This does not send anything to the client.
handleRequest :: Message -> IO Message
handleRequest _ =
    return $ MsgResponse RespError

initClient :: ClientId -> Socket -> IO Client
initClient ident sock =
    Client <$> return ident
           <*> return sock
           <*> newTChanIO

initNetwork :: IO Socket
initNetwork = do
    let hints   = defaultHints { addrFlags = [AI_PASSIVE] }
    serverAddr <- head <$> getAddrInfo (Just hints) Nothing (Just $ show radiodPort)
    sock       <- socket (addrFamily serverAddr) Stream defaultProtocol

    bind sock (addrAddress serverAddr)
    listen sock 1
    return sock

initServer :: IO Server
initServer =
    Server <$> newTVarIO Map.empty
           <*> (loadConfigFile >>= newIORef)
           <*> newIORef Map.empty

insertClient :: Server -> Client -> STM ()
insertClient Server{..} client@Client{..} =
    modifyTVar' serverClients $ Map.insert clientId client

-- A thread to handle serving one client - do whatever init is required, go into
-- a loop handling its requests, and then clean up when the client closes its end.
-- This is basically a wrapper around serveLoop.
serve :: Server -> ClientId -> Socket -> IO ()
serve server@Server{..} ident sock = do
    client <- initClient ident sock
    CES.bracket_ (atomically $ insertClient server client)
                 (atomically $ deleteClient server client)
                 (serveLoop server client)

-- Handle the network traffic for a single client.
serveLoop :: Server -> Client -> IO ()
serveLoop Server{..} Client{..} = do
    -- This thread handles reading traffic from the network for a single client.
    -- It then processes the Request message and puts it into the channel for the
    -- client.  This channel is not the network - it's a way of communicating
    -- between threads in this server.
    let rx = forever $ decode <$> recv clientSocket 1024 >>= \case
            Right msg -> handleRequest msg >>= atomically . writeTChan clientSendChan

            -- There was an error decoding the received message, but the client is
            -- expecting something in return, so we hav eto give it something to read.
            -- Have an error.
            Left _    -> atomically $ writeTChan clientSendChan (MsgResponse RespError)

    -- And this thread takes whatever's in the client channel and writes it to the
    -- network, making sure the client gets it.
    let tx = forever $ do
         msg <- atomically $ readTChan clientSendChan
         sendAll clientSocket (encode msg)

    -- Run both until one thread dies, and then kill the other one too.  This includes if
    -- a thread dies due to an exception.  However, the exception will be re-rasied and
    -- needs to be caught higher up.
    void $ concurrently rx tx

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

handler :: Server -> Event -> IO ()
handler server@Server{..} Created{isDirectory=False, filePath=fp} = do
    devMap <- readIORef serverDeviceMap
    let fp' = "/dev" </> C8.unpack fp

    case Map.lookup (T.pack fp') devMap of
        Just (Rig ty port descr) -> do h <- startRigctld fp' ty port
                                       insert serverProcMap (T.pack fp') h
                                       broadcast server (MsgResponse $ RespRadioConnected (T.unpack descr, port))
        Just (Rot ty port descr) -> do h <- startRotctld fp' ty port
                                       insert serverProcMap (T.pack fp') h
                                       broadcast server (MsgResponse $ RespRotorConnected (T.unpack descr, port))
        _                    -> return ()

handler server@Server{..} Deleted{isDirectory=False, filePath=fp} = do
    devMap <- readIORef serverDeviceMap
    let fp' = "/dev" </> C8.unpack fp

    case Map.lookup (T.pack fp') devMap of
        Just (Rig _ port descr) -> do procMap <- readIORef serverProcMap
                                      case Map.lookup (T.pack fp') procMap of
                                          Nothing -> return ()
                                          Just h  -> do stopRigctld h
                                                        delete serverProcMap (T.pack fp')
                                                        broadcast server (MsgResponse $ RespRadioDisconnected (T.unpack descr, port))
        Just (Rot _ port descr) -> do procMap <- readIORef serverProcMap
                                      case Map.lookup (T.pack fp') procMap of
                                          Nothing -> return ()
                                          Just h  -> do stopRotctld h
                                                        delete serverProcMap (T.pack fp')
                                                        broadcast server (MsgResponse $ RespRotorDisconnected (T.unpack descr, port))
        _                -> return ()

handler _ _ = return ()

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

-- Loop forever, accepting connections from clients and spawning a server thread for each one.
-- This runs in a separate async.  When it terminates, the whole program should be shut down.
acceptConns :: Socket -> Server -> IO ()
acceptConns sock server =
    forM_ [1 .. ] $ \ident -> do
        (conn, _) <- accept sock
        a         <- async $ serve server ident conn
        -- Any exceptions that happen while handling the client process will be caught here, making
        -- sure the server process continues.
        void $ waitCatch a
        -- The connection might already be closed, in which case we should just ignore the failure
        -- but should prevent it from killing the server.
        close conn `CES.catchIOError` \_ -> return ()

program :: IO ()
program = do
    server@Server{..} <- initServer
    inotify           <- initINotify
    sock              <- initNetwork

    void $ do
        acceptThr <- async $ acceptConns sock server

        void $ installHandler sigCHLD Ignore Nothing
        void $ installHandler sigHUP  (Catch $ loadConfigFile >>= atomicWriteIORef serverDeviceMap) Nothing
        void $ installHandler sigTERM (CatchOnce $ cancel acceptThr) Nothing

        void $ addWatch inotify [Create, Delete] "/dev" (handler server)

        void $ waitCatch acceptThr

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
