module NewBookForm.State (..) where

import Effects exposing (Effects)
import Material.Button as Button
import Material.Snackbar as Snackbar
import Material.Textfield as Textfield
import String
import Task

import Generated.Api exposing (postBooks, Book)
import Lib.Effects exposing (pure, andThen)
import NewBookForm.Types exposing (..)
import RatingField.State


init : Model
init =
  let
    tfModel =
      Textfield.model
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
    , rating = Nothing
    , ratingField =
        RatingField.State.init
    , submitButton =
        Button.model True
    , snackbar =
        Snackbar.model
    }


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    CreateBook ->
      checkCreateBook model

    TitleFieldAction action' ->
      let
        newField =
          Textfield.update action' model.titleField

        ( title, error ) =
          if String.isEmpty newField.value then
            ( Nothing
            , Just "Title should not be empty"
            )
          else
            ( Just newField.value
            , Nothing
            )
      in
        pure
          { model
            | titleField = { newField | error = error }
            , title = title
          }

    AuthorNameFieldAction action' ->
      let
        newField =
          Textfield.update action' model.authorNameField

        ( authorName, error ) =
          if String.isEmpty newField.value then
            ( Nothing
            , Just "Author name should not be empty"
            )
          else
            ( Just newField.value
            , Nothing
            )
      in
        pure
          { model
            | authorNameField = { newField | error = error }
            , authorName = authorName
          }

    AuthorYearOfBirthFieldAction action' ->
      let
        newField =
          Textfield.update action' model.authorYearOfBirthField

        ( year, error ) =
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
        pure
          { model
            | authorYearOfBirthField = { newField | error = error }
            , authorYearOfBirth = year
          }

    RatingFieldAction action' ->
      let
        newField =
          RatingField.State.update action' model.ratingField

      in
        pure
          { model
            | ratingField = newField
            , rating = Just newField.rating
          }

    FetchBooks ->
      -- Should be handled by the parent
      pure model

    SubmitButtonAction action' ->
      let
        ( newButton, buttonFx ) =
          Button.update action' model.submitButton
      in
        ( { model | submitButton = newButton }
        , Effects.map SubmitButtonAction buttonFx
        )

    SnackbarAction action' ->
      let
        ( newSnackbar, sbFx ) =
          Snackbar.update action' model.snackbar
      in
        ( { model | snackbar = newSnackbar }
        , Effects.map SnackbarAction sbFx
        )


checkCreateBook : Model -> ( Model, Effects Action )
checkCreateBook model =
  case validate model of
    Just book ->
      createBook book
        `andThen` popupMessage "Book created!"

    Nothing ->
      popupMessage
        "Book was not created because of validation errors."
        model


createBook : Book -> ( Model, Effects Action )
createBook book =
  ( init
  , postBooks book
      |> Task.toMaybe
      |> Task.map (\_ -> FetchBooks)
      |> Effects.task
  )


popupMessage : String -> Model -> ( Model, Effects Action )
popupMessage msg model =
  let
    ( newSnackbar, sbFx ) =
      Snackbar.add
        (Snackbar.toast () msg)
        model.snackbar
  in
    ( { model | snackbar = newSnackbar }
    , Effects.map SnackbarAction sbFx
    )


validate : Model -> Maybe Book
validate model =
  Maybe.map4
    (\title authorName authorYearOfBirth rating ->
      { bookId = Nothing
      , title = title
      , author =
          { name = authorName
          , yearOfBirth = authorYearOfBirth
          }
      , rating = rating
      }
    )
    model.title
    model.authorName
    model.authorYearOfBirth
    model.rating
