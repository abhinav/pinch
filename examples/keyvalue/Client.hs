{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Main (main) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

import Control.Monad
import Control.Monad.Catch  (Exception, throwM)
import Data.ByteString      (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Char            (isSpace)
import Data.Function        (fix)
import Data.Maybe
import Data.Text            (Text)
import Data.Text.Encoding   (encodeUtf8)
import Data.Typeable        (Typeable)

import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types  as HTTP
import qualified Pinch               as P

import Types


data ClientError
    = HTTPError HTTP.Status ByteString
    | ThriftProtocolError String
  deriving (Show, Eq, Typeable)

instance Exception ClientError


keyValueClient :: String -> HTTP.Manager -> KeyValue IO
keyValueClient url manager = KeyValue
    { getValue = mkRequest "getValue"
    , setValue = mkRequest "setValue"
    }
  where
    baseRequest = fromMaybe (error "Invalid URL") (HTTP.parseUrl url)

    encode = P.encodeMessage P.binaryProtocol
    decode = P.decodeMessage P.binaryProtocol

    mkRequest :: forall req res.
        ( P.Pinchable req, P.Tag req ~ P.TStruct
        , P.Pinchable res, P.Tag res ~ P.TStruct
        ) => Text -> req -> IO res
    mkRequest name call = do
        res <- HTTP.httpLbs req manager

        let status = HTTP.responseStatus res
            body = toStrict (HTTP.responseBody res)

        unless (HTTP.statusIsSuccessful status) $
            throwM (HTTPError status body)

        case decode body >>= P.getMessageBody of
            Left err -> throwM (ThriftProtocolError err)
            Right reply -> return reply
      where
        message = P.mkMessage name P.Call 0 call
        req = baseRequest
            { HTTP.method = "POST"
            , HTTP.requestBody = HTTP.RequestBodyBS $ encode message
            }


main :: IO ()
main = do
    client <- keyValueClient "http://localhost:8080"
          <$> HTTP.newManager HTTP.defaultManagerSettings

    fix $ \loop -> do
        line <- TIO.getLine
        case T.split isSpace line of
            ["quit"] -> return ()

            ["get", key] -> do
                res <- getValue client (GetValueRequest (P.putField key))
                case res of
                    GetValueSuccess value  -> print value
                    GetValueDoesNotExist _ -> putStrLn "No matching value."
                loop

            ["set", key, value] -> do
                let v = ValueBin (P.putField (encodeUtf8 value))
                _ <- setValue client $
                     SetValueRequest (P.putField key) (P.putField v)
                loop

            _ -> putStrLn "Invalid command" >> loop
