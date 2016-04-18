module NewBookForm (Model, init, Action(FetchBooks), update, view) where

import Effects exposing (Effects)
import Material.Button as Button
import Material.Grid as Grid exposing (grid, cell, offset, size, Device(..))
import Material.Textfield as Textfield
import Html exposing (div, form, text, h2)
import Html.Attributes exposing (class)
import String
import Task

import Events exposing (onSubmitPreventDefault)
import Generated.Api exposing (..)
import Lib exposing (pure)


type alias Model =
  { title : Maybe String
  , titleField : Textfield.Model
  , authorName : Maybe String
  , authorNameField : Textfield.Model
  , authorYearOfBirth : Maybe Int
  , authorYearOfBirthField : Textfield.Model
  , submitButton : Button.Model
  }


init : Model
init =
  let
    tfModel = Textfield.model
  in
    { title = Nothing
    , titleField =
        { tfModel | label = Just { text = "Title", float = True } }
    , authorName = Nothing
    , authorNameField =
        { tfModel | label = Just { text = "Author", float = True } }
    , authorYearOfBirth = Nothing
    , authorYearOfBirthField =
        { tfModel | label = Just { text = "Author's YOB", float = True } }
    , submitButton =
        Button.model True
    }


type Action
  = CreateBook
  | FetchBooks
  | TitleFieldAction Textfield.Action
  | AuthorNameFieldAction Textfield.Action
  | AuthorYearOfBirthFieldAction Textfield.Action
  | SubmitButtonAction Button.Action


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    CreateBook ->
      createBook model

    TitleFieldAction action' ->
      let
        newField =
          Textfield.update action' model.titleField
        (title, error) =
          if String.isEmpty newField.value then
            ( Nothing
            , Just "Title should not be empty"
            )
          else
            ( Just newField.value
            , Nothing
            )
      in
        pure { model
               | titleField = { newField | error = error }
               , title = title
             }

    AuthorNameFieldAction action' ->
      let
        newField =
          Textfield.update action' model.authorNameField
        ( authorName, error) =
          if String.isEmpty newField.value then
            ( Nothing
            , Just "Author name should not be empty"
            )
          else
            ( Just newField.value
            , Nothing
            )
      in
        pure { model
               | authorNameField = { newField | error = error }
               , authorName = authorName
             }

    AuthorYearOfBirthFieldAction action' ->
      let
        newField =
          Textfield.update action' model.authorYearOfBirthField
        (year, error) =
          case String.toInt newField.value of
            Err msg ->
              ( Nothing
              , Just msg
              )
            Ok val ->
              ( Just val
              , Nothing
              )
      in
        pure { model
               | authorYearOfBirthField = { newField | error = error }
               , authorYearOfBirth = year
             }

    FetchBooks ->
      -- Should be handled by the parent
      pure model

    -- TODO match only click action
    SubmitButtonAction action' ->
      let
        (newButton, buttonFx) =
          Button.update action' model.submitButton
      in
        ( model
        , Effects.map SubmitButtonAction buttonFx
        )


createBook : Model -> (Model, Effects Action)
createBook model =
  case validate model of
    Just (title, authorName, authorYearOfBirth) ->
      ( init
      , postBooks
          { bookId = Nothing
          , title = title
          , author =
              { name = authorName
              , yearOfBirth = authorYearOfBirth
              }
          }
          |> Task.toMaybe
          |> Task.map (\_ -> FetchBooks)
          |> Effects.task
      )
    Nothing ->
      pure model


validate : Model -> Maybe (String, String, Int)
validate model =
  Maybe.map3 (,,)
    model.title
    model.authorName
    model.authorYearOfBirth


view : Signal.Address Action -> Model -> Html.Html
view address model =
  grid []
    [ cell [ size All 12 ]
        [ h2 [] [ text "Create a book" ] ]
    , cell [ size All 12 ]
        [ form
            [ onSubmitPreventDefault address CreateBook ]
            [ viewFormFields address model ]
        ]
    ]


viewFormFields address model =
  grid []
    [ cell [size All 3]
        [ Textfield.view (Signal.forwardTo address TitleFieldAction) model.titleField [] ]
    , cell [size All 3]
        [ Textfield.view (Signal.forwardTo address AuthorNameFieldAction) model.authorNameField [] ]
    , cell [size All 3]
        [ Textfield.view (Signal.forwardTo address AuthorYearOfBirthFieldAction) model.authorYearOfBirthField [] ]
    , cell [size All 3]
        [ Button.raised (Signal.forwardTo address SubmitButtonAction) model.submitButton []
            [ text "Create book" ]
        ]
    ]
