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

import           Data.Aeson   (FromJSON, ToJSON)
import qualified Data.Map.Strict as Map
import           Elm          (ToElmType)
import           GHC.Generics
import           Servant      ((:<|>), (:>), ReqBody, Post, Get, JSON)

data Author = Author
  { name        :: String
  , yearOfBirth :: Int
  } deriving (Show, Generic)

instance ToElmType Author
instance ToJSON Author
instance FromJSON Author

type BookId = String

data Book = Book
  { bookId :: Maybe BookId
  , title  :: String
  , author :: Author
  } deriving (Show, Generic)

type BookDB = Map.Map BookId Book

instance ToElmType Book
instance ToJSON Book
instance FromJSON Book


type Api = "books" :> ( Get '[JSON] [Book]
                   :<|> ReqBody '[JSON] Book :> Post '[JSON] Book
                      )
