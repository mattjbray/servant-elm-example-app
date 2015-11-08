module Main where

import Effects exposing (Effects)
import Html
import Html.Events
import StartApp
import Task
import Json.Decode as Json

import Generated.Api as Api


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
  { counter : Int }


init : (Model, Effects Action)
init = ({ counter = 0 }, Effects.none)


type Action
  = IncCounter
  | FetchCounter
  | SetCounter (Maybe Int)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    IncCounter ->
      incCounter model

    FetchCounter ->
      fetchCounter model

    SetCounter mNewCount ->
      setCounter mNewCount model


incCounter : Model -> (Model, Effects Action)
incCounter model =
  ( { model | counter <- model.counter + 1 }
  , Api.postCounterInc decodeCounter
    |> Task.toMaybe
    |> Effects.task
    |> Effects.map SetCounter )


fetchCounter : Model -> (Model, Effects Action)
fetchCounter model =
  ( model
  , Api.getCounter decodeCounter
    |> Task.toMaybe
    |> Effects.task
    |> Effects.map SetCounter )


setCounter : Maybe Int -> Model -> (Model, Effects Action)
setCounter mNewCount model =
  ( case mNewCount of
      Just newCount ->
        { model | counter <- newCount }
      Nothing ->
        model
  , Effects.none )


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div [] [ Html.text (toString model.counter)
              , Html.button [Html.Events.onClick address FetchCounter] [Html.text "refresh"]
              , Html.button [Html.Events.onClick address IncCounter] [Html.text "inc"]
              ]


decodeCounter : Json.Decoder Int
decodeCounter = Json.int
