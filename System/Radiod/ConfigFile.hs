{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Radiod.ConfigFile(loadConfigFile,
                                parseConfigFile)
 where

import qualified Data.Map as Map
import           Data.Maybe(mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Safe(atMay)
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
    parseOneRig :: [T.Text] -> Maybe (T.Text, Device)
    parseOneRig l = readMaybe (T.unpack $ l !! 2) >>= \ty -> do
        let port = atMay l 3 >>= Just . T.unpack >>= readMaybe
        Just (head l, Rig ty port)

    parseOneRotor :: [T.Text] -> Maybe (T.Text, Device)
    parseOneRotor l = readMaybe (T.unpack $ l !! 2) >>= \ty -> do
        let port = atMay l 3 >>= Just . T.unpack >>= readMaybe
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
