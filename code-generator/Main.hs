{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Proxy  (Proxy (Proxy))
import           Elm         (Spec (Spec), specsToDir)
import           Servant.Elm (ElmOptions (..), defElmImports, defElmOptions,
                              generateElmForAPIWith)

import           Api.Types   (Api)

elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = "http://localhost:8000/api" }

specs :: [Spec]
specs =
  [ Spec ["Generated", "Api"] $
         defElmImports : generateElmForAPIWith elmOpts (Proxy :: Proxy Api)
  ]

main :: IO ()
main = specsToDir specs "frontend/src"
