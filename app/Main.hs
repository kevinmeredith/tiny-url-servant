{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

type API = "tinyUrl" :> Capture "value" String :> Get '[JSON] ResolvedTinyUrl

newtype TinyUrl = TinyUrl String deriving Generic

instance ToJSON TinyUrl

data ResolvedTinyUrl = ResolvedTinyUrl
  { value :: TinyUrl } deriving Generic

instance ToJSON ResolvedTinyUrl

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