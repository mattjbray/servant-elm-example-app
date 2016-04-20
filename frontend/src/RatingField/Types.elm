module RatingField.Types (..) where

type alias Model =
  { rating : Int
  , active : Maybe Int
  }

type Action
  = SetRating Int
  | SetActive (Maybe Int)
