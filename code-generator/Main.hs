{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Proxy  (Proxy (Proxy))
import           Elm         (Spec (Spec), specsToDir, toElmTypeSource,
                              toElmDecoderSource, toElmEncoderSource)
import           Servant.Elm (ElmOptions (..), defElmImports, defElmOptions,
                              generateElmForAPIWith, UrlPrefix (Static))

import           Api.Types   (Api, Author, Book)

elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Static "http://localhost:8000/api" }

specs :: [Spec]
specs =
  [ Spec ["Generated", "Api"]
         (defElmImports
          : toElmTypeSource    (Proxy :: Proxy Author)
          : toElmDecoderSource (Proxy :: Proxy Author)
          : toElmEncoderSource (Proxy :: Proxy Author)
          : toElmTypeSource    (Proxy :: Proxy Book)
          : toElmDecoderSource (Proxy :: Proxy Book)
          : toElmEncoderSource (Proxy :: Proxy Book)
          : generateElmForAPIWith elmOpts  (Proxy :: Proxy Api))
  ]

main :: IO ()
main = specsToDir specs "frontend/src"
