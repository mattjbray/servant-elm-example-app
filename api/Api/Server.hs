module Api.Server
    ( server
    ) where

import           Control.Concurrent.STM (TVar, atomically, readTVar, readTVarIO,
                                         writeTVar)
import           Control.Monad.IO.Class (liftIO)
import           Servant                ((:<|>) ((:<|>)), Server)

import           Api.Types              (Api)


server :: TVar Int -> Server Api
server counter = postCounterInc :<|> getCounter
  where getCounter = liftIO (readTVarIO counter)
        postCounterInc = liftIO . atomically $ do
          count <- readTVar counter
          writeTVar counter (count + 1)
          return (count + 1)
