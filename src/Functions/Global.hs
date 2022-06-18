{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Functions.Global ( 
    writeLog ) where

import Data.DateTime ( 
    getCurrentTime,
    addSeconds )

import Data.Text ( 
    pack,
    unpack,
    Text (..) )

import System.Directory (
    createDirectory )

import Control.Exception (
    Exception,
    SomeException (..),
    try )

-- to log writing
writeLog :: Text -> IO ()
writeLog log =
    (try (
    getCurrentTime >>= (return . addSeconds 10800) >>= \timeOpen ->
    appendFile "./resource/logs.log" (show timeOpen ++ ": " ++ unpack log ++ "\n")
    ) :: IO (Either SomeException ())) >>= \solve ->
    case solve of
    Left e ->
        putStrLn "WARNING: could not write log" >>
        createDirectory "./resource" >>
        writeLog (pack "resource created")
    Right () ->
        return ()
