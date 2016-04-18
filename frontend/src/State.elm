module State (..) where

import Effects exposing (Effects)
import Material.Button as Button
import Material.Layout as Layout
import Task

import Generated.Api exposing (getBooks)
import Lib.Effects exposing (pure)
import NewBookForm.State
import NewBookForm.Types

import Types exposing (..)


init : ( Model, Effects Action )
init =
  fetchBooks initModel


initModel : Model
initModel =
  { books = []
  , newBookForm = NewBookForm.State.init
  , mdlLayout = Layout.defaultLayoutModel
  , refreshButton = Button.model True
  }


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    SetBooks mNewBooks ->
      pure { model | books = Maybe.withDefault model.books mNewBooks }

    NewBookFormAction (NewBookForm.Types.FetchBooks) ->
      fetchBooks model

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

    RefreshButtonAction action' ->
      let
        (newButton, buttonFx) =
          Button.update action' model.refreshButton
        (newModel, fx) =
          case action' of
            Button.Click ->
              fetchBooks model
            _ ->
              pure model
      in
        ( { newModel | refreshButton = newButton }
        , Effects.batch
            [ Effects.map RefreshButtonAction buttonFx
            , fx
            ]
        )


fetchBooks : Model -> ( Model, Effects Action )
fetchBooks model =
  ( model
  , getBooks
      |> Task.toMaybe
      |> Effects.task
      |> Effects.map SetBooks
  )
