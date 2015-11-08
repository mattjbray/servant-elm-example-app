{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( Api
    , server
    ) where

import Servant ((:>), (:<|>)((:<|>)), Get, Post, JSON, Proxy(Proxy), Server)

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (TVar, readTVarIO, writeTVar, atomically, readTVar)

type Counter = Int

type Api = "counter" :> "inc" :> Post '[JSON] Counter
      :<|> "counter" :> Get '[JSON] Counter


server :: TVar Int -> Server Api
server counter = postCounterInc :<|> getCounter
  where getCounter = liftIO (readTVarIO counter)
        postCounterInc = liftIO . atomically $ do
          count <- readTVar counter
          writeTVar counter (count + 1)
          return (count + 1)
