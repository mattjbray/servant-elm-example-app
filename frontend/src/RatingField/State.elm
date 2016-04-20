module RatingField.State (..) where

import RatingField.Types exposing (..)


init : Model
init =
  { rating = 1
  , active = Nothing
  }


update : Action -> Model -> Model
update action model =
  case action of
    SetRating rating ->
      { model | rating = rating }

    SetActive active ->
      { model | active = active }
