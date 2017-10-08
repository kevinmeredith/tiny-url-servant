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
import Servant.HTML.Lucid
import Control.Concurrent.MVar
import Data.Map
import Control.Monad.Except

type API = "tinyUrl" :> Capture "value" String :> (
              Get '[JSON, HTML] ResolvedTinyUrl :<|> ReqBody '[JSON] UpdatedTinyUrl :> Put '[] NoContent
            )

-- credit: https://stackoverflow.com/questions/46390448/deriving-tohtml-for-newtype
newtype TinyUrl = TinyUrl String deriving (Generic, ToHtml, Ord, Eq)

instance ToJSON TinyUrl

newtype ResolvedTinyUrl = ResolvedTinyUrl { value :: TinyUrl } deriving Generic

data UpdatedTinyUrl = UpdatedTinyUrl
  { k :: TinyUrl, v :: String } deriving Generic

instance ToJSON ResolvedTinyUrl

instance FromJSON UpdatedTinyUrl

instance ToHtml ResolvedTinyUrl where
  toHtml x =
    tr_ $ do
      td_ (toHtml $ value x)

  toHtmlRaw = toHtml

newtype ResolvedUrls = ResolvedUrls (MVar (Map TinyUrl String))

tinyUrlAPI :: Proxy API
tinyUrlAPI = Proxy

-- Handler a = ExceptT ServantErr IO a

-- Handler ResolvedTinyUrl

server :: IO (MVar (Map TinyUrl String)) -> Server API
server ioMap = get :<|> put

  where get :: String -> Handler ResolvedTinyUrl
        get s = Handler $ do
          map    <- lift $ ioMap
          m      <- lift $ takeMVar map
          found  <- lift $ return $ Data.Map.lookup (TinyUrl s) m
          case found of
             Just a  -> return $ ResolvedTinyUrl (TinyUrl a)
             Nothing -> (lift $ putStrLn ("did not find " ++ s)) >> throwError err404

        put :: UpdatedTinyUrl -> Handler ()
        put (UpdatedTinyUrl key value) = Handler $ do
         map     <- lift $ ioMap
         m       <- lift $ takeMVar map
         updated <- lift $ Data.Map.insert key value m
         _       <- lift $ putMVar map updated
         return NoContent


app :: IO (MVar (Map TinyUrl String)) -> Application
app map = serve tinyUrlAPI (server map)

main :: IO ()
main = run 8081 $ app (newMVar $ Data.Map.insert (TinyUrl "foo") "bar" $ Data.Map.empty)