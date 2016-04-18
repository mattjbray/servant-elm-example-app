module State (..) where

import Effects exposing (Effects)
import Material.Layout as Layout

import BooksList.State
import BooksList.Types
import NewBookForm.State
import NewBookForm.Types

import Types exposing (..)


init : ( Model, Effects Action )
init =
  let
    (booksList, blFx) =
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
    -- NewBookFormAction (NewBookForm.Types.FetchBooks) ->
    --   fetchBooks model

    BooksListAction action' ->
      let
        (newBooksList, blFx) =
          BooksList.State.update action' model.booksList
      in
        ( { model | booksList = newBooksList }
        , Effects.map BooksListAction blFx
        )

    NewBookFormAction action' ->
      let
        (newBookForm', fx) =
          NewBookForm.State.update action' model.newBookForm
      in
        ( { model | newBookForm = newBookForm' }
        , Effects.map NewBookFormAction fx
        )

    MDLLayout action' ->
        let
          (mdlLayout', fx) =
            Layout.update action' model.mdlLayout
        in
          ( { model | mdlLayout = mdlLayout' }, Effects.map MDLLayout fx )
