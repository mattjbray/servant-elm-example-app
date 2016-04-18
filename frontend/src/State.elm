module State (..) where

import Effects exposing (Effects)
import Material.Layout as Layout

import Lib.Effects exposing (pure)

import BooksList.State
import BooksList.Types
import NewBookForm.State
import NewBookForm.Types

import Types exposing (..)


init : ( Model, Effects Action )
init =
  let
    ( booksList, blFx ) =
      BooksList.State.init
  in
    ( { booksList = booksList
      , newBookForm = NewBookForm.State.init
      , mdlLayout = Layout.defaultLayoutModel
      }
    , Effects.map BooksListAction blFx
    )


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    BooksListAction action' ->
      let
        ( newBooksList, blFx ) =
          BooksList.State.update action' model.booksList
      in
        ( { model | booksList = newBooksList }
        , Effects.map BooksListAction blFx
        )

    NewBookFormAction action' ->
      let
        ( newBookForm', bfFx ) =
          NewBookForm.State.update action' model.newBookForm

        -- Thread the FetchBooks action through to the books list.
        ( newBooksList, blFx ) =
          case action' of
            NewBookForm.Types.FetchBooks ->
              BooksList.State.update BooksList.Types.FetchBooks model.booksList

            _ ->
              pure model.booksList
      in
        ( { model
            | newBookForm = newBookForm'
            , booksList = newBooksList
          }
        , Effects.batch
            [ Effects.map NewBookFormAction bfFx
            , Effects.map BooksListAction blFx
            ]
        )

    MDLLayout action' ->
      let
        ( mdlLayout', fx ) =
          Layout.update action' model.mdlLayout
      in
        ( { model | mdlLayout = mdlLayout' }, Effects.map MDLLayout fx )
