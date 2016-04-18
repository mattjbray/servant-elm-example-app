module Main (..) where

import Effects exposing (Effects)
import Html exposing (div, button, text, input, p, h1, h2, form)
import Html.Attributes exposing (placeholder, value, type', class)
import Html.Events exposing (onClick)
import Material
import Material.Button as Button
import Material.Grid as Grid exposing (grid, cell, offset, size, Device(..))
import Material.Layout
import Material.Textfield
import StartApp
import String
import Task
import Json.Decode as Json
import Events exposing (onChange, onEnter, onSubmitPreventDefault)

import Generated.Api exposing (..)
import Lib exposing (pure)
import NewBookForm


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs =
      [ Material.Layout.setupSignals MDLLayout ]
    }


main : Signal Html.Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


type alias Model =
  { books : List Book
  , newBookForm : NewBookForm.Model
  , mdlLayout : Material.Layout.Model
  , refreshButton : Button.Model
  }


init : ( Model, Effects Action )
init =
  fetchBooks initModel


initModel : Model
initModel =
  { books = []
  , newBookForm = NewBookForm.init
  , mdlLayout = Material.Layout.defaultLayoutModel
  , refreshButton = Button.model True
  }


type Action
  = SetBooks (Maybe (List Book))
  | NewBookFormAction NewBookForm.Action
  | MDLLayout Material.Layout.Action
  | RefreshButtonAction Button.Action


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    SetBooks mNewBooks ->
      pure { model | books = Maybe.withDefault model.books mNewBooks }

    NewBookFormAction (NewBookForm.FetchBooks) ->
      fetchBooks model

    NewBookFormAction action' ->
      let
        (newBookForm', fx) =
          NewBookForm.update action' model.newBookForm
      in
        ( { model | newBookForm = newBookForm' }
        , Effects.map NewBookFormAction fx
        )

    MDLLayout action' ->
        let
          (mdlLayout', fx) =
            Material.Layout.update action' model.mdlLayout
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


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Material.Layout.view (Signal.forwardTo address MDLLayout) model.mdlLayout
      { header =
          [ Material.Layout.row
              [ Material.Layout.title "Books"
              ]
          ]
      , drawer = []
      , tabs = []
      , main = viewMain address model
      }


viewMain address model =
  [ grid []
      [ cell [ offset All 2, size All 8 ]
          [ NewBookForm.view (Signal.forwardTo address NewBookFormAction) model.newBookForm ]
      , cell [ offset All 2, size All 8 ]
          [ viewBookList address model ]
      ]
  ]


viewBookList address model =
  div
    []
    [ h2 [] [ text "All books" ]
    , div
        [ class "row" ]
        (List.map viewBook model.books)
    , Button.raised (Signal.forwardTo address RefreshButtonAction) model.refreshButton []
        [ text "Refresh book list" ]
    ]


viewBook : Book -> Html.Html
viewBook book =
  div
    [ class "col-lg-3" ]
    [ div
        [ class "panel panel-default" ]
        [ div
            [ class "panel-heading" ]
            [ text book.title ]
        , div
            [ class "panel-body" ]
            [ p
                []
                [ text
                    (book.author.name
                      ++ " (b."
                      ++ toString book.author.yearOfBirth
                      ++ ")"
                      ++ " {"
                      ++ Maybe.withDefault "unknown" book.bookId
                      ++ "}"
                    )
                ]
            ]
        ]
    ]
