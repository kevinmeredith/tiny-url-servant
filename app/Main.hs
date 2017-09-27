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

type API = "tinyUrl" :> Capture "value" String :> Get '[JSON, HTML] ResolvedTinyUrl

-- credit: https://stackoverflow.com/questions/46390448/deriving-tohtml-for-newtype
newtype TinyUrl = TinyUrl String deriving (Generic, ToHtml)

instance ToJSON TinyUrl

data ResolvedTinyUrl = ResolvedTinyUrl
  { value :: TinyUrl } deriving Generic

instance ToJSON ResolvedTinyUrl

instance ToHtml ResolvedTinyUrl where
  toHtml x =
    tr_ $ do
      td_ (toHtml $ value x)

  toHtmlRaw = toHtml

newtype ResolvedUrls = ResolvedUrls (MVar (Map TinyUrl String))

tinyUrlAPI :: Proxy API
tinyUrlAPI = Proxy

server :: (MVar (Map TinyUrl String)) -> Server API
server map = f
  where f :: String -> Handler ResolvedTinyUrl
        f s = do
          m      <- liftM $ takeMVar map
          found  <- return $ Data.Map.lookup s m
          case found of
             Just a  -> return $ ResolvedTinyUrl $ TinyUrl a
             Nothing -> return NotFound

app :: (MVar (Map TinyUrl String)) -> Application
app map = serve tinyUrlAPI (server map)

main :: IO ()
main = run 8081 app