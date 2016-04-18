module View (..) where

import Html exposing (div, text, p, h2, h3)
import Material.Button as Button
import Material.Grid as Grid exposing (grid, cell, offset, size, Device(..))
import Material.Layout as Layout

import Generated.Api exposing (Book)
import NewBookForm.View

import Types exposing (..)


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Layout.view (Signal.forwardTo address MDLLayout) model.mdlLayout
      { header =
          [ Layout.row
              [ Layout.title "Books"
              ]
          ]
      , drawer = []
      , tabs = []
      , main = viewMain address model
      }


viewMain address model =
  [ grid []
      [ cell [ offset All 2, size All 8 ]
          [ NewBookForm.View.view (Signal.forwardTo address NewBookFormAction) model.newBookForm ]
      , cell [ offset All 2, size All 8 ]
          [ viewBookList address model ]
      ]
  ]


viewBookList address model =
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
