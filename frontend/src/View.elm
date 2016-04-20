module View (..) where

import Html
import Material.Grid as Grid exposing (grid, cell, noSpacing, offset, size, Device(..))
import Material.Layout as Layout
import Material.Style as Style

import Lib.Style exposing (nestedGridStylesheet)

import BooksList.View
import NewBookForm.View

import Types exposing (..)


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div
    []
    [ nestedGridStylesheet
    , Layout.view
        (Signal.forwardTo address MDLLayout)
        model.mdlLayout
        { header =
            [ Layout.row
                [ Layout.title "Books"
                ]
            ]
        , drawer = []
        , tabs = []
        , main = viewMain address model
        }
    ]


cellStyle : List Style.Style
cellStyle =
  [ offset Desktop 2
  , size Desktop 8
  , offset Tablet 1
  , size Tablet 10
  ]


viewMain : Signal.Address Action -> Model -> List Html.Html
viewMain address model =
  [ grid
      [ noSpacing ]
      [ cell
          cellStyle
          [ NewBookForm.View.view (Signal.forwardTo address NewBookFormAction) model.newBookForm ]
      , cell
          cellStyle
          [ BooksList.View.view (Signal.forwardTo address BooksListAction) model.booksList ]
      ]
  ]
