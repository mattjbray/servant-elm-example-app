{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Types
    ( Api
    ) where

import           Servant ((:<|>), (:>), Get, JSON, Post)


type Counter = Int


type Api = "counter" :> "inc" :> Post '[JSON] Counter
      :<|> "counter" :> Get '[JSON] Counter
