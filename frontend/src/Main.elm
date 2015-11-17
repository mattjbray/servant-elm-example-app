module Main where

import Effects exposing (Effects)
import Html
import Html.Attributes
import Html.Events
import StartApp
import String
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
      pure { model | books <- Maybe.withDefault model.books mNewBooks }

    SetNewBookTitle title ->
      pure { model | newBookTitle <- title }

    SetNewBookAuthorName name ->
      pure { model | newBookAuthorName <- name }

    SetNewBookAuthorYearOfBirth year ->
      pure { model | newBookAuthorYearOfBirth <- year }

    CreateBook ->
      if validate model
        then ( { model | newBookTitle <- "", newBookAuthorName <- "" }
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
  Html.div []
    [ viewBookForm address model
    , Html.button [Html.Events.onClick address FetchBooks] [Html.text "refresh"]
    , Html.div [] (List.map viewBook model.books)
    ]


viewBookForm : Signal.Address Action -> Model -> Html.Html
viewBookForm address model =
  Html.div []
    [ Html.input
        [ Html.Attributes.placeholder "Title"
        , Html.Attributes.value model.newBookTitle
        , Events.onChange address SetNewBookTitle
        , Events.onEnter address CreateBook] []
    , Html.input
        [ Html.Attributes.placeholder "Author"
        , Html.Attributes.value model.newBookAuthorName
        , Events.onChange address SetNewBookAuthorName
        , Events.onEnter address CreateBook] []
    , Html.input
        [ Html.Attributes.placeholder "Date of birth"
        , Html.Attributes.value (toString model.newBookAuthorYearOfBirth)
        , Html.Attributes.type' "number"
        , Events.onChange address (SetNewBookAuthorYearOfBirth << Maybe.withDefault 0 << Result.toMaybe << String.toInt)
        , Events.onEnter address CreateBook] []
    ]


viewBook : Book -> Html.Html
viewBook book =
  Html.p []
    [Html.text
       (book.title
        ++ " by "
        ++ book.author.name
        ++ " (b." ++ toString book.author.yearOfBirth ++ ")"
        ++ " {" ++ Maybe.withDefault "unknown" book.bookId ++ "}")]


pure : a -> (a, Effects b)
pure model = (model, Effects.none)
