module BooksList.View (..) where

import Html exposing (div, text, p, h2, h4)
import Material.Button as Button
import Material.Grid as Grid exposing (grid, cell, size, Device(..))
import Material.Style as Style

import Generated.Api exposing (Book)
import Lib.Style exposing (nestedGrid)

import BooksList.Types exposing (..)


view address model =
  grid
    []
    [ cell
        [ size All 12 ]
        [ h2 [] [ text "All books" ] ]
    , cell
        [ size All 12 ]
        [ grid
            [ nestedGrid ]
            (List.map viewBook model.books)
        ]
    , cell
        [ size All 12 ]
        [ Button.raised
            (Signal.forwardTo address RefreshButtonAction)
            model.refreshButton
            []
            [ text "Refresh book list" ]
        ]
    ]


viewBook : Book -> Grid.Cell
viewBook book =
  cell
    [ size All 4 ]
    [ h4 [] [ text book.title ]
    , p
        []
        [ text
            (book.author.name
              ++ " (b."
              ++ toString book.author.yearOfBirth
              ++ ") ("
              ++ toString book.rating
              ++ " stars) {"
              ++ Maybe.withDefault "unknown" book.bookId
              ++ "}"
            )
        ]
    ]
