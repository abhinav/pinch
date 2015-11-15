{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

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

import Types

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

    handleMessage m = case P.messageName m of
        "getValue" -> case P.getMessageBody m of
            Left err -> handleError err
            Right (req :: GetValueRequest) -> do
                res <- getValue store req
                let reply = P.mkMessage "getValue" P.ReplyMessage mid res
                respondSuccess $ encode reply
        "setValue" -> case P.getMessageBody m of
            Left err -> handleError err
            Right (req :: SetValueRequest) -> do
                res <- setValue store req
                let reply = P.mkMessage "setValue" P.ReplyMessage mid res
                respondSuccess $ encode reply
        name -> handleError $ "Unknown method: " ++ show name
        -- TODO use TApplicationException format for this
      where
        mid = P.messageId m

main :: IO ()
main = newValueStore >>= \store -> run 8080 (app store)
