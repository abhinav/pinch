{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Main (main) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

import Data.ByteString.Lazy       (fromStrict, toStrict)
import Data.ByteString.Lazy.Char8 (pack)
import Data.IORef                 (IORef)
import Data.Map.Strict            (Map)
import Data.Text                  (Text)
import Network.Wai.Handler.Warp   (run)

import qualified Data.IORef         as IORef
import qualified Data.Map.Strict    as Map
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai        as HTTP
import qualified Pinch              as P

import Types hiding (KeyValue (..))

type ValueStore = IORef (Map Text Value)

newValueStore :: IO ValueStore
newValueStore = IORef.newIORef Map.empty

getValue :: ValueStore -> GetValueRequest -> IO GetValueResponse
getValue store req = do
    m <- IORef.readIORef store
    return $! case key `Map.lookup` m of
        Nothing ->
            GetValueDoesNotExist . P.putField $
            KeyDoesNotExist (P.putField Nothing)
        Just value ->
            GetValueSuccess (P.putField value)
  where
    GetValueRequest{getValueKey = P.Field key} = req

setValue :: ValueStore -> SetValueRequest -> IO SetValueResponse
setValue store req = do
    IORef.atomicModifyIORef' store (\m -> (Map.insert key value m, ()))
    return $ SetValueSuccess P.Void
  where
    SetValueRequest
        { setValueKey = P.Field key
        , setValueValue = P.Field value
        } = req

app :: ValueStore -> HTTP.Application
app store request respond = do
    requestBody <- toStrict <$> HTTP.strictRequestBody request
    either handleError handleMessage $ decode requestBody
  where
    encode = P.encodeMessage P.binaryProtocol
    decode = P.decodeMessage P.binaryProtocol

    respondSuccess = respond . HTTP.responseLBS HTTP.status200 [] . fromStrict
    handleError err = respond $ HTTP.responseLBS HTTP.status500 [] (pack err)

    handle
        :: forall req res.
            ( P.Pinchable req, P.Tag req ~ P.TStruct
            , P.Pinchable res, P.Tag res ~ P.TStruct
            )
        => (ValueStore -> req -> IO res) -> P.Message
        -> IO HTTP.ResponseReceived
    handle f msg = case P.getMessageBody msg of
        Left err -> handleError err
        Right (req :: req) -> do
            res <- f store req
            respondSuccess . encode $
                P.mkMessage messageName P.Reply messageId res
      where
        messageName = P.messageName msg
        messageId = P.messageId msg

    handleMessage m = case P.messageName m of
        "getValue" -> handle getValue m
        "setValue" -> handle setValue m
        name -> handleError $ "Unknown method: " ++ show name
        -- TODO use TApplicationException format for this

main :: IO ()
main = newValueStore >>= \store -> run 8080 (app store)
