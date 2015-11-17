{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Types
    ( Api
    , Author(..)
    , Book(..)
    , BookId
    , BookDB
    ) where

import           Data.Aeson   (ToJSON)
import qualified Data.Map.Strict as Map
import           Elm          (ToElmType)
import           GHC.Generics
import           Servant      ((:>), Get, JSON)

data Author = Author
  { name        :: String
  , yearOfBirth :: Int
  } deriving (Show, Generic)

instance ToElmType Author
instance ToJSON Author


type BookId = Int

data Book = Book
  { bookId :: BookId
  , title  :: String
  , author :: Author
  } deriving (Show, Generic)

type BookDB = Map.Map BookId Book

instance ToElmType Book
instance ToJSON Book


type Api = "books" :> Get '[JSON] [Book]
