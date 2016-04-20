module RatingField.View (..) where

import Html exposing (span)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseOver, onMouseOut)
import Material.Icon as Icon
import RatingField.Types exposing (..)


view : Signal.Address Action -> Model -> Html.Html
view address model =
  span
    [ onMouseOut address (SetActive Nothing) ]
    (List.map (viewStar address model) [1..5])


viewStar : Signal.Address Action -> Model -> Int -> Html.Html
viewStar address model index =
  span
    [ onMouseOver address (SetActive (Just index))
    , onClick address (SetRating index)
    , style
        [ ( "cursor", "pointer" ) ]
    ]
    [ Icon.i
        (case model.active of
          Just active ->
            if index <= active then
              "star"
            else if index <= model.rating then
              "star_half"
            else
              "star_border"

          Nothing ->
            if index <= model.rating then
              "star"
            else
              "star_border"
        )
    ]
