module BooksList.View (..) where

import Html exposing (div, text, p, h2, h3)
import Material.Button as Button

import Generated.Api exposing (Book)

import BooksList.Types exposing (..)


view address model =
  div
    []
    [ h2 [] [ text "All books" ]
    , div
        []
        (List.map viewBook model.books)
    , Button.raised (Signal.forwardTo address RefreshButtonAction) model.refreshButton []
        [ text "Refresh book list" ]
    ]


viewBook : Book -> Html.Html
viewBook book =
  div
    []
    [ h3 [] [text book.title]
    , p
        []
        [ text
            (book.author.name
              ++ " (b."
              ++ toString book.author.yearOfBirth
              ++ ")"
              ++ " {"
              ++ Maybe.withDefault "unknown" book.bookId
              ++ "}"
            )
        ]
    ]
