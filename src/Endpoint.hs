{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Monad law, left identity" #-}

module Endpoint ( 
    generalRoundPol ) where


import qualified Network.HTTP.Simple as HTTPS (
    httpLBS,
    parseRequest,
    getResponseBody,
    Response (..) )

import qualified Data.Yaml as Y (
    decode )

import qualified Data.ByteString.Char8 as B8 (
    readFile,
    ByteString (..) )

import qualified Data.ByteString.Lazy.Char8 as BL8 ( 
    pack,
    ByteString (..) )

import BotTypes ( 
    Config (..) )

import qualified Data.Text as T ( 
    pack,
    unpack,
    Text (..) )

import qualified Data.Aeson as JSON (
    decode,
    Object (..),
    Value (..),
    Array (..) )

import Data.Aeson.Types ( 
    parseMaybe,
    (.:) )

import Data.Aeson.KeyMap as KM ( 
    lookup )

import Data.Aeson.Key ( 
    fromString )

import Functions.Global ( 
    writeLog )

import Control.Exception (
    Exception,
    SomeException (..),
    try )

import Control.Concurrent ( 
    threadDelay )

import qualified Data.Vector as V ( 
    last,
    unsafeLast,
    head,
    length )

import Data.Scientific ( 
    Scientific (..) )


-- round pol
generalRoundPol :: IO ()
generalRoundPol =
    putStrLn "Open to listen" >>
    writeLog (T.pack "open to listen") >>
    listener >>
    putStrLn "Finish to sleep" >>
    writeLog (T.pack "sleep")

-- event listener
listener :: IO ()
listener =
    putStrLn "Listening..." >>
    writeLog (T.pack "ready to listen, reading config") >>
    (try (B8.readFile "./resource/config.yaml") :: IO (Either SomeException B8.ByteString)) >>= \tryReadCong ->
    case tryReadCong of
    Left e ->
        putStrLn "WARNING: no configs" >>
        writeLog (T.pack "WARNING: no configs") >>
        threadDelay 4000000 >>
        listener
    Right readedBytes ->
        return readedBytes >>= \byte ->
        let 
        parsed = Y.decode byte :: Maybe Config
        in
        process parsed

-- :: IO (Either SomeException ()) for every exception
-- process reader
process :: Maybe Config -> IO ()
process parsed =
    case parsed of
    Nothing ->
        putStrLn "Problems with config.yaml" >>
        writeLog (T.pack "problems with config") >>
        listener
    Just ( Config key timeout host port ) ->
        (try (
            HTTPS.parseRequest ("https://api.telegram.org/bot" ++ key ++ "/getUpdates") >>=
            HTTPS.httpLBS 
        ) :: IO (Either SomeException (HTTPS.Response BL8.ByteString))) >>= \tryCatchUpdates ->
        case tryCatchUpdates of
        Left e -> 
            putStrLn "WARNING: could not send request" >>
            writeLog (T.pack "WARNING: could not send request") >>
            threadDelay (timeout * 2) >>
            process parsed
        Right succsessUpdates ->
            return succsessUpdates >>= \updates ->
            let
            body = HTTPS.getResponseBody updates
            bodyObject = JSON.decode body :: Maybe JSON.Object
            in
            case bodyObject of
            Nothing ->
                putStrLn "Nothing" >>
                writeLog (T.pack "WARNING: could not parse data object") >>
                threadDelay timeout >>
                listener
            Just obj ->
                useResult key host port (searchThrowObj "result" obj) >>
                threadDelay timeout >>
                process parsed

-- using result of map parsing
useResult :: String -> String -> Int -> Maybe JSON.Value -> IO ()
useResult keyy host port mbResult =
    case mbResult of
    Just result ->
        case result of
        JSON.Array resultArr -> 
            case V.length resultArr of
            0 -> return ()
            _ ->
                case V.last resultArr of
                JSON.Object lastUpdateObj ->
                    case searchThrowObj "message" lastUpdateObj of
                    Just message ->
                        case message of
                        JSON.Object messageObj ->
                            case searchThrowObj "chat" messageObj of
                            Just chat ->
                                case chat of
                                JSON.Object chatObj ->
                                    case searchThrowObj "id" chatObj of
                                    Just chatIdMb ->
                                        case chatIdMb of
                                        JSON.Number chatId ->
                                            case searchThrowObj "text" messageObj of
                                            Just textOfMes ->
                                                case textOfMes of
                                                JSON.String textOfMesStr ->
                                                    case searchThrowObj "from" messageObj of
                                                    Just from ->
                                                        case from of
                                                        JSON.Object fromObj ->
                                                            case searchThrowObj "username" fromObj of
                                                            Just username ->
                                                                case username of 
                                                                JSON.String unStr ->
                                                                    finalEvent fromObj lastUpdateObj chatId unStr textOfMesStr keyy host port
                                                                _ -> 
                                                                    noParseWarn "could not parse username" >>
                                                                    lastUpdateRem keyy lastUpdateObj
                                                            Nothing -> 
                                                                finalEvent fromObj lastUpdateObj chatId "?user?" textOfMesStr keyy host port
                                                        _ -> 
                                                            noParseWarn "could not parse from object of message" >>
                                                            lastUpdateRem keyy lastUpdateObj
                                                    Nothing -> 
                                                        noParseWarn "could not parse 'from' of message" >>
                                                        lastUpdateRem keyy lastUpdateObj
                                                _ -> 
                                                    noParseWarn "could not parse string of message" >>
                                                    lastUpdateRem keyy lastUpdateObj
                                            Nothing -> 
                                                noParseWarn "no text of message" >>
                                                lastUpdateRem keyy lastUpdateObj
                                        _ -> 
                                            noParseWarn "could not parse chat id" >>
                                            lastUpdateRem keyy lastUpdateObj
                                    Nothing -> 
                                        noParseWarn "could not get chat id" >>
                                        lastUpdateRem keyy lastUpdateObj
                                _ -> 
                                    noParseWarn "no parse chat" >>
                                    lastUpdateRem keyy lastUpdateObj
                            Nothing -> 
                                noParseWarn "no chat object" >>
                                lastUpdateRem keyy lastUpdateObj
                        _ -> 
                            noParseWarn "no message data" >>
                            lastUpdateRem keyy lastUpdateObj
                    Nothing -> 
                        noParseWarn "no message" >>
                        lastUpdateRem keyy lastUpdateObj
                _ -> noParseWarn "no last update"
        _ -> noParseWarn "no result array"
    Nothing -> noParseWarn "wrong result"

-- final event to perform
finalEvent :: JSON.Object -> JSON.Object -> Scientific -> T.Text  -> T.Text -> String -> String -> Int -> IO ()
finalEvent fromObj lastUpdateObj chatId unStr textOfMesStr keyy host port =
    case searchThrowObj "is_bot" fromObj of
    Just isBot ->
        case isBot of
        JSON.Bool isBotBool ->
            case searchThrowObj "update_id" lastUpdateObj of
            Just updateId ->
                case updateId of
                JSON.Number updateIdNum ->
                    executer keyy host port chatId updateIdNum unStr textOfMesStr isBotBool
                _ -> 
                    noParseWarn "could not parse update id number" >>
                    lastUpdateRem keyy lastUpdateObj
            Nothing -> 
                noParseWarn "no update id" >>
                lastUpdateRem keyy lastUpdateObj
        _ -> 
            noParseWarn "could not parse data about bot" >>
            lastUpdateRem keyy lastUpdateObj
    Nothing -> 
        noParseWarn "no data about bot" >>
        lastUpdateRem keyy lastUpdateObj

-- special to do and remove last update
executer :: String -> String -> Int -> Scientific -> Scientific -> T.Text -> T.Text -> Bool -> IO ()
executer keyy host port chatId updateIdNum unStr textOfMesStr isBotBool =
    let 
    -- find sollution for integer
    doubleId = truncate updateIdNum
    doubleChat = truncate chatId
    in
    useImportant unStr doubleChat textOfMesStr isBotBool host port >>
    (try (
        HTTPS.parseRequest ("https://api.telegram.org/bot" ++ keyy ++ "/getUpdates?offset=" ++ (show $ succ doubleId)) >>=
        HTTPS.httpLBS 
    ) :: IO (Either SomeException (HTTPS.Response BL8.ByteString))) >>= \tryCatchUpdates ->
    case tryCatchUpdates of
    Left e -> 
        noParseWarn "could not send request"
    Right succsessUpdates ->
        return ()

-- last update remover
lastUpdateRem :: String -> JSON.Object -> IO ()
lastUpdateRem keyy lastUpdateObj =
    case searchThrowObj "update_id" lastUpdateObj of
    Just updateId ->
        case updateId of
        JSON.Number updateIdNum ->
            let 
            doubleId = truncate updateIdNum
            in
            (try (
                HTTPS.parseRequest ("https://api.telegram.org/bot" ++ keyy ++ "/getUpdates?offset=" ++ (show $ succ doubleId)) >>=
                HTTPS.httpLBS 
            ) :: IO (Either SomeException (HTTPS.Response BL8.ByteString))) >>= \tryCatchUpdates ->
            case tryCatchUpdates of
            Left e -> 
                noParseWarn "could not send request"
            Right succsessUpdates ->
                return ()
        _ -> noParseWarn "could not parse update id number"
    Nothing -> noParseWarn "no update id"

-- no parsing warning
noParseWarn :: String -> IO ()
noParseWarn warn =
    putStrLn ("WARNING: " ++ warn) >>
    writeLog (T.pack ("WARNING: " ++ warn))

-- use important data
useImportant :: T.Text -> Integer -> T.Text -> Bool -> String -> Int -> IO ()
useImportant un chatId txt bot host port =
    writeLog (T.pack (T.unpack un ++ " say's [" ++ T.unpack txt ++ "] bot: " ++ show bot))

-- searcher in object
searchThrowObj :: String -> JSON.Object -> Maybe JSON.Value
searchThrowObj keyy =
    KM.lookup (fromString keyy)
