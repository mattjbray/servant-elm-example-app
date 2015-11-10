{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}

module Main where

import           Control.Concurrent.STM   (TVar, newTVarIO)
import           Lucid                    (Html, body_, doctypehtml_, head_,
                                           script_, src_, title_)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant                  ((:<|>) ((:<|>)), (:>), Get,
                                           Proxy (Proxy), Raw, Server, serve,
                                           serveDirectory)
import           Servant.HTML.Lucid       (HTML)

import qualified Api.Server
import qualified Api.Types

type SiteApi =  "api" :> Api.Types.Api
            :<|> Get '[HTML] (Html ())
            :<|> "assets" :> Raw

siteApi :: Proxy SiteApi
siteApi = Proxy

server :: TVar Int -> Server SiteApi
server counter = apiServer :<|> home :<|> assets
  where home = return homePage
        apiServer = Api.Server.server counter
        assets = serveDirectory "frontend/dist"

homePage :: Html ()
homePage =
  doctypehtml_ $ do
    head_ $ do
      title_ "Example Servant-Elm App"
      script_ [src_ "assets/app.js"] ""
    body_ (script_ "var elmApp = Elm.fullscreen(Elm.Main)")

app :: TVar Int -> Application
app counter = serve siteApi (server counter)

main :: IO ()
main = do
  let port = 8000
  counter <- newTVarIO 0
  putStrLn $ "Serving on port " ++ show port ++ "..."
  run port (app counter)
