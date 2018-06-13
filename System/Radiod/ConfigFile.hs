{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Radiod.ConfigFile(loadConfigFile,
                                parseConfigFile)
 where

import qualified Data.Map as Map
import           Data.Maybe(mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Directory(doesFileExist)
import           System.FilePath((</>))
import           Text.Read(readMaybe)

import Paths_radiod(getSysconfDir)
import System.Radiod.Types

loadConfigFile :: IO DeviceMap
loadConfigFile = do
    dir <- getSysconfDir
    c   <- doesFileExist (dir </> "radiod.conf") >>= \case
               True  -> TIO.readFile (dir </> "radiod.conf")
               False -> return ""

    return $ parseConfigFile c

parseConfigFile :: T.Text -> DeviceMap
parseConfigFile str | strs <- T.lines str =
    Map.fromList $ mapMaybe parseOneLine strs
 where
    parseOneRig :: [Maybe T.Text] -> Maybe (T.Text, Device)
    parseOneRig [Just deviceNode, _, Just model, port, Just descr] =
        readMaybe (T.unpack model) >>= \ty -> do
            let port' = port >>= Just . T.unpack >>= readMaybe
            Just (deviceNode, Rig ty port' descr)

    parseOneRig _ = Nothing

    parseOneRotor :: [Maybe T.Text] -> Maybe (T.Text, Device)
    parseOneRotor [Just deviceNode, _, Just model, port, Just descr] =
        readMaybe (T.unpack model) >>= \ty -> do
            let port' = port >>= Just . T.unpack >>= readMaybe
            Just (deviceNode, Rot ty port' descr)

    parseOneRotor _ = Nothing

    parseOneLine :: T.Text -> Maybe (T.Text, Device)
    parseOneLine input = let
        stripped = T.strip input
     in
        if T.null stripped || "#" `T.isPrefixOf` stripped then Nothing
        else let
            l = map (\x -> if T.null x then Nothing else Just x)
                    (T.split (== ',') stripped)
         in
            case l of
                [_, Just "rig", _, _, _] -> parseOneRig l
                [_, Just "rot", _, _, _] -> parseOneRotor l
                _                        -> Nothing
