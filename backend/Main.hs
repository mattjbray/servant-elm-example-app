{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}

module Main where

import Control.Concurrent.STM (TVar, newTVarIO)
import qualified Lib
import           Lucid                    (Html, doctypehtml_, head_,
                                           script_, src_, title_, body_)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant                  ((:<|>) ((:<|>)), (:>), Get,
                                           Proxy (Proxy), Server, Raw, serve, serveDirectory)
import           Servant.HTML.Lucid       (HTML)

type FullApi =  "api" :> Lib.Api
            :<|> Get '[HTML] (Html ())
            :<|> "assets" :> Raw

fullApi :: Proxy FullApi
fullApi = Proxy

server :: TVar Int -> Server FullApi
server counter = apiServer :<|> home :<|> assets
  where home = return homePage
        apiServer = Lib.server counter
        assets = serveDirectory "frontend/dist"

homePage :: Html ()
homePage =
  doctypehtml_ $ do
    head_ $ do
      title_ "Example Servant-Elm App"
      script_ [src_ "assets/app.js"] ""
    body_ (script_ "var elmApp = Elm.fullscreen(Elm.Main)")

app :: TVar Int -> Application
app counter = serve fullApi (server counter)

main :: IO ()
main = do
  let port = 8000 :: Integer
  counter <- newTVarIO 0
  putStrLn $ "Serving on port " ++ show port ++ "..."
  run 8000 (app counter)
