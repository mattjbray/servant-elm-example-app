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
  { counter : Counter }


init : (Model, Effects Action)
init = ({ counter = {count=0} }, Effects.none)


type Action
  = IncCounter
  | FetchCounter
  | SetCounter (Maybe Counter)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    IncCounter ->
      incCounter model

    FetchCounter ->
      fetchCounter model

    SetCounter mNewCounter ->
      ( {model | counter <- Maybe.withDefault model.counter mNewCounter }
      , Effects.none
      )


incCounter : Model -> (Model, Effects Action)
incCounter model =
  ( model -- { model | counter <- model.counter + 1 }
  , postCounterInc
    |> Task.toMaybe
    |> Effects.task
    |> Effects.map SetCounter )


fetchCounter : Model -> (Model, Effects Action)
fetchCounter model =
  ( model
  , getCounter
    |> Task.toMaybe
    |> Effects.task
    |> Effects.map SetCounter )


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div [] [ Html.text (toString model.counter)
              , Html.button [Html.Events.onClick address FetchCounter] [Html.text "refresh"]
              , Html.button [Html.Events.onClick address IncCounter] [Html.text "inc"]
              ]
