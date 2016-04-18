module BooksList.State (..) where

import Effects exposing (Effects)
import Material.Button as Button
import Task

import Generated.Api exposing (getBooks)
import Lib.Effects exposing (pure)

import BooksList.Types exposing (..)


init : ( Model, Effects Action )
init =
  fetchBooks initModel


initModel : Model
initModel =
  { books = []
  , refreshButton = Button.model True
  }


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    SetBooks mNewBooks ->
      pure { model | books = Maybe.withDefault model.books mNewBooks }

    RefreshButtonAction action' ->
      let
        ( newButton, buttonFx ) =
          Button.update action' model.refreshButton

        ( newModel, fx ) =
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

    FetchBooks ->
      fetchBooks model


fetchBooks : Model -> ( Model, Effects Action )
fetchBooks model =
  ( model
  , getBooks
      |> Task.toMaybe
      |> Effects.task
      |> Effects.map SetBooks
  )
