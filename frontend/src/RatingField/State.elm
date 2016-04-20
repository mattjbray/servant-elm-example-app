module RatingField.State (..) where

import RatingField.Types exposing (..)


init : Int -> Model
init rating =
  { rating = rating
  , active = Nothing
  }


update : Action -> Model -> Model
update action model =
  case action of
    SetRating rating ->
      { model | rating = rating }

    SetActive active ->
      { model | active = active }
