module Api.Server
    ( server
    ) where

import           Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict        as Map
import           Data.UUID              (toString)
import           Servant                ((:<|>) ((:<|>)), Server)
import           System.Random          (randomIO)

import           Api.Types              (Api, Book(bookId), BookDB)


server :: TVar BookDB -> Server Api
server tBookDb = listBooks :<|> createBook
  where listBooks =
          liftIO . atomically $ do
            bookDb <- readTVar tBookDb
            return (Map.elems bookDb)
        createBook book = do
          uuid <- liftIO randomIO
          liftIO . atomically $ do
            let idStr = toString uuid
                bookWithId = book { bookId = Just (toString uuid) }
            bookDb <- readTVar tBookDb
            writeTVar tBookDb (Map.insert idStr bookWithId bookDb)
            return bookWithId
