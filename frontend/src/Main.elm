module Main where

import Effects exposing (Effects)
import Html
import Html.Events
import StartApp
import Task
import Json.Decode as Json

import Events
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
  { books : List Book
  , newBookTitle : String }


init : (Model, Effects Action)
init = ({ books = [], newBookTitle = "" }, Effects.none)


type Action
  = FetchBooks
  | SetBooks (Maybe (List Book))
  | SetNewBookTitle String
  | CreateBook


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    FetchBooks ->
      fetchBooks model

    SetBooks mNewBooks ->
      pure { model | books <- Maybe.withDefault model.books mNewBooks }

    SetNewBookTitle title ->
      pure { model | newBookTitle <- title }

    CreateBook ->
      ( { model | newBookTitle <- "" }
      , postBooks {bookId = Nothing, title = model.newBookTitle, author = {name = "", yearOfBirth = 0}}
          |> Task.toMaybe
          |> Task.map (\_ -> FetchBooks)
          |> Effects.task
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
  Html.div []
    [ Html.div [] [Html.text (toString model.books)]
    , Html.button [Html.Events.onClick address FetchBooks] [Html.text "refresh"]
    , viewBookForm address model
    ]


viewBookForm : Signal.Address Action -> Model -> Html.Html
viewBookForm address model =
  Html.div []
    [ Html.input
        [ Events.onChange address SetNewBookTitle
        , Events.onEnter address CreateBook] [] ]


pure : a -> (a, Effects b)
pure model = (model, Effects.none)
