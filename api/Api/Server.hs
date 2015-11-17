module Api.Server
    ( server
    ) where

import           Control.Concurrent.STM (STM, TVar, atomically, readTVar,
                                         readTVarIO, writeTVar)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import           Servant                ((:<|>) ((:<|>)), Server)

import           Api.Types              (Api, Author, Book, BookId, BookDB)




server :: TVar BookDB -> Server Api
server tBookDb = listBooks
  where listBooks =
          liftIO . atomically $ do
            bookDb <- readTVar tBookDb
            return (Map.elems bookDb)
