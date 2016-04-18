module View (..) where

import Html
import Material.Grid as Grid exposing (grid, cell, offset, size, Device(..))
import Material.Layout as Layout

import BooksList.View
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
          [ BooksList.View.view (Signal.forwardTo address BooksListAction) model.booksList ]
      ]
  ]
