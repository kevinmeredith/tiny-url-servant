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

data ResolvedUrls = ResolvedUrls (MVar (Map TinyUrl String))

tinyUrlAPI :: Proxy API
tinyUrlAPI = Proxy

server :: Server API
server = f
  where f :: String -> Handler ResolvedTinyUrl
        f x = return $ ResolvedTinyUrl $ TinyUrl x

app :: Application
app = serve tinyUrlAPI server

main :: IO ()
main = run 8081 app