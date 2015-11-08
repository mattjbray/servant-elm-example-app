{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO   as TIO
import           Servant        (Proxy (Proxy))
import           Servant.JS     (CommonGeneratorOptions,
                                 defCommonGeneratorOptions, jsForAPI,
                                 moduleName, urlPrefix)
import           Servant.JS.Elm (elmJSWith)

import qualified Api.Types

api :: Proxy Api.Types.Api
api = Proxy

elmOpts :: CommonGeneratorOptions
elmOpts = defCommonGeneratorOptions
  { moduleName = "Generated.Api"
  , urlPrefix = "http://localhost:8000/api"
  }

main :: IO ()
main = TIO.putStr $ jsForAPI api (elmJSWith elmOpts)
