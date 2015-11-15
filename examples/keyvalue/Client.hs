{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Main (main) where

import Data.ByteString.Lazy (toStrict)
import Data.Char            (isSpace)
import Data.Function        (fix)
import Data.Text            (Text)
import Data.Text.Encoding   (encodeUtf8)

import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types  as HTTP
import qualified Pinch               as P

import Types

main :: IO ()
main = do
    manager <- HTTP.newManager HTTP.defaultManagerSettings
    baseRequest <- HTTP.parseUrl "http://localhost:8080"

    let encode = P.encodeMessage P.binaryProtocol
        decode = P.decodeMessage P.binaryProtocol

        mkRequest :: forall req res.
            ( P.Pinchable req, P.Pinchable res
            , P.Tag req ~ P.TStruct
            , P.Tag res ~ P.TStruct
            ) => Text -> req -> IO (Maybe res)
        mkRequest name call = do
            res <- HTTP.httpLbs req manager
            if not (HTTP.statusIsSuccessful (HTTP.responseStatus res))
                then return Nothing
                else case decode (toStrict $ HTTP.responseBody res) of
                    Left _ -> return Nothing
                    Right msg -> case P.getMessageBody msg of
                        Left _ -> return Nothing
                        Right (reply :: res) -> return (Just reply)
          where
            message = P.mkMessage name P.CallMessage 0 call
            req = baseRequest
                { HTTP.method = "POST"
                , HTTP.requestBody = HTTP.RequestBodyBS . encode $ message
                }

    fix $ \loop -> do
        line <- TIO.getLine
        case T.split isSpace line of
            ["quit"] -> return ()
            ["get", key] -> do
                res <- mkRequest "getValue" $
                    GetValueRequest (P.putField key)
                case res of
                    Just (GetValueSuccess value) -> print value
                    Just (GetValueDoesNotExist _) -> putStrLn "No matching value."
                    Nothing -> putStrLn "error making request"
                loop
            ["set", key, value] -> do
                let v = ValueBin (P.putField (encodeUtf8 value))
                res <- mkRequest "setValue" $
                    SetValueRequest (P.putField key) (P.putField v)
                case res of
                    Just (SetValueSuccess _) -> putStrLn "Done"
                    Nothing -> putStrLn "error making request"
                loop
            _ -> putStrLn "Invalid command" >> loop
