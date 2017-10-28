{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Prelude ()
import Prelude.Compat
import Data.Aeson.Types
import GHC.Generics
import Lucid
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Docs hiding (API)
import Servant.HTML.Lucid
import Control.Concurrent.MVar
import Data.Map
import Control.Monad.Except
import Network.URI
import qualified Data.Text as T


type API = "tinyUrl" :> ValueAPI

type ValueAPI = Capture "value" String :> (
                       Get '[JSON, HTML] TinyUrlValue
                  :<|> ReqBody '[JSON] TinyUrlValue :> PutNoContent '[JSON] NoContent
        )

newtype TinyUrlValue = TinyUrlValue { value :: URI } deriving Generic

instance FromJSON URI where
    parseJSON = withText "URI" $ \t ->
        case (parseURI . T.unpack $ t) of
          Just uri -> return uri
          Nothing  -> fail "Invalid URI"

instance ToJSON URI where
  toJSON x = String $ T.pack $ show x

instance FromJSON TinyUrlValue
instance ToJSON TinyUrlValue

instance ToHtml TinyUrlValue where
  toHtml x =
    tr_ $ do
      td_ (toHtml $ show $ value x)

  toHtmlRaw = toHtml

newtype ResolvedUrls = ResolvedUrls (MVar (Map String URI))

tinyUrlAPI :: Proxy API
tinyUrlAPI = Proxy

server :: MVar (Map String URI) -> Server API
server map = tinyUrlOperations

  where tinyUrlOperations key =
          get key :<|> put key

          where get :: String -> Handler TinyUrlValue
                get key = Handler $ do
                  m      <- lift $ readMVar map
                  _      <- lift $ putStrLn ("m " ++ show m)
                  found  <- lift $ return $ Data.Map.lookup key m
                  case found of
                     Just a  -> return $ TinyUrlValue a
                     Nothing -> (lift $ putStrLn ("did not find " ++ key)) >> throwError err404

                put :: String -> TinyUrlValue -> Handler NoContent
                put key (TinyUrlValue uri) = Handler $ do
                     m       <- lift $ takeMVar map
                     updated <- lift $ return $ Data.Map.insert key uri m
                     _       <- lift $ putMVar map updated
                     return NoContent

instance ToCapture (Capture "value" String) where
  toCapture _ =
    DocCapture "key"
               "TinyUrl key"

instance (ToSample TinyUrlValue) where
  toSamples _ = singleSample $ TinyUrlValue $ URI "https" (Just $ URIAuth "" "www.google.com" ":42") "/bippy" "" ""


-- TinyUrlValue

app :: MVar (Map String URI) -> Application
app map = serve tinyUrlAPI (server map)

main :: IO ()
main =  do
  map <- newMVar $ Data.Map.empty
  run 8081 (app map)
