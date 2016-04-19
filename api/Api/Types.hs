{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import           Elm          (ElmType)
import           GHC.Generics (Generic)
import           Servant      ((:<|>), (:>), ReqBody, Post, Get, JSON)

data Author = Author
  { name        :: String
  , yearOfBirth :: Int
  } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

type BookId = String

data Book = Book
  { bookId :: Maybe BookId
  , title  :: String
  , author :: Author
  , rating :: Int
  } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

type BookDB = Map.Map BookId Book


type Api = "books" :> ( Get '[JSON] [Book]
                   :<|> ReqBody '[JSON] Book :> Post '[JSON] Book
                      )
