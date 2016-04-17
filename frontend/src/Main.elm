module Main where

import Effects exposing (Effects)
import Html exposing (div, button, text, input, p, h1)
import Html.Attributes exposing (placeholder, value, type', class)
import Html.Events exposing (onClick)
import StartApp
import String
import Task
import Json.Decode as Json

import Events exposing (onChange, onEnter)
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
  , newBookTitle : String
  , newBookAuthorName : String
  , newBookAuthorYearOfBirth : Int }


init : (Model, Effects Action)
init = fetchBooks initModel

initModel : Model
initModel = { books = []
            , newBookTitle = ""
            , newBookAuthorName = ""
            , newBookAuthorYearOfBirth = 0 }

type Action
  = FetchBooks
  | SetBooks (Maybe (List Book))
  | SetNewBookTitle String
  | SetNewBookAuthorName String
  | SetNewBookAuthorYearOfBirth Int
  | CreateBook


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    FetchBooks ->
      fetchBooks model

    SetBooks mNewBooks ->
      pure { model | books = Maybe.withDefault model.books mNewBooks }

    SetNewBookTitle title ->
      pure { model | newBookTitle = title }

    SetNewBookAuthorName name ->
      pure { model | newBookAuthorName = name }

    SetNewBookAuthorYearOfBirth year ->
      pure { model | newBookAuthorYearOfBirth = year }

    CreateBook ->
      if validate model
        then ( { model | newBookTitle = "", newBookAuthorName = "" }
             , postBooks { bookId = Nothing
                         , title = model.newBookTitle
                         , author = { name = model.newBookAuthorName
                                    , yearOfBirth = model.newBookAuthorYearOfBirth } }
                 |> Task.toMaybe
                 |> Task.map (\_ -> FetchBooks)
                 |> Effects.task
             )
        else pure model


fetchBooks : Model -> (Model, Effects Action)
fetchBooks model =
  ( model
  , getBooks
    |> Task.toMaybe
    |> Effects.task
    |> Effects.map SetBooks )


validate : Model -> Bool
validate {newBookTitle, newBookAuthorName} =
  List.all (not << String.isEmpty) [newBookTitle, newBookAuthorName]


view : Signal.Address Action -> Model -> Html.Html
view address model =
  div [class "container-fluid"]
    [ h1 [] [text "Books"]
    , viewBookForm address model
    , button [onClick address FetchBooks] [text "refresh"]
    , div [class "row"] (List.map viewBook model.books)
    ]


viewBookForm : Signal.Address Action -> Model -> Html.Html
viewBookForm address model =
  div [class "row"]
    [ div [class "col-lg-12"]
        [ input
            [ placeholder "Title"
            , value model.newBookTitle
            , onChange address SetNewBookTitle
            , onEnter address CreateBook] []
        , input
            [ placeholder "Author"
            , value model.newBookAuthorName
            , onChange address SetNewBookAuthorName
            , onEnter address CreateBook] []
        , input
            [ placeholder "Date of birth"
            , value (toString model.newBookAuthorYearOfBirth)
            , type' "number"
            , onChange address (SetNewBookAuthorYearOfBirth << Maybe.withDefault 0 << Result.toMaybe << String.toInt)
            , onEnter address CreateBook] []
        ]
    ]


viewBook : Book -> Html.Html
viewBook book =
  div [class "col-lg-3"]
    [ div [class "panel panel-default"]
        [ div [class "panel-heading"]
            [text book.title]
        , div [class "panel-body"]
            [ p []
                [text
                   (book.author.name
                    ++ " (b." ++ toString book.author.yearOfBirth ++ ")"
                    ++ " {" ++ Maybe.withDefault "unknown" book.bookId ++ "}")]
            ]
        ]
    ]


pure : a -> (a, Effects b)
pure model = (model, Effects.none)
