module Main where

import Effects exposing (Effects)
import Html
import Html.Events
import StartApp
import Task
import Json.Decode as Json

import Generated.Api exposing (..)


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = []
    }


main : Signal Html.Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


type alias Model =
  { books : List Book }


init : (Model, Effects Action)
init = ({ books = [] }, Effects.none)


type Action
  = FetchBooks
  | SetBooks (Maybe (List Book))


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    FetchBooks ->
      fetchBooks model

    SetBooks mNewBooks ->
      ( { model | books <- Maybe.withDefault model.books mNewBooks }
      , Effects.none
      )


fetchBooks : Model -> (Model, Effects Action)
fetchBooks model =
  ( model
  , getBooks
    |> Task.toMaybe
    |> Effects.task
    |> Effects.map SetBooks )


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div [] [ Html.text (toString model.books)
              , Html.button [Html.Events.onClick address FetchBooks] [Html.text "refresh"]
              ]
