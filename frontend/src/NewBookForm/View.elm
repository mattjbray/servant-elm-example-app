module NewBookForm.View (..) where

import Html exposing (form, text, h2)
import Material.Button as Button
import Material.Grid as Grid exposing (grid, cell, size, Device(..))
import Material.Textfield as Textfield
import Material.Snackbar as Snackbar

import Lib.Events exposing (onSubmitPreventDefault)
import Lib.Style exposing (nestedGrid)

import NewBookForm.Types exposing (..)


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div
    []
    [ grid
       []
       [ cell
           [ size All 12 ]
           [ h2 [] [ text "Create a book" ] ]
       , cell
           [ size All 12 ]
           [ form
               [ onSubmitPreventDefault address CreateBook ]
               [ viewFormFields address model ]
           ]
       ]
    , Snackbar.view (Signal.forwardTo address SnackbarAction) model.snackbar
    ]


viewFormFields address model =
  grid
    [ nestedGrid ]
    [ cell
        [ size All 3 ]
        [ Textfield.view (Signal.forwardTo address TitleFieldAction) model.titleField [] ]
    , cell
        [ size All 3 ]
        [ Textfield.view (Signal.forwardTo address AuthorNameFieldAction) model.authorNameField [] ]
    , cell
        [ size All 3 ]
        [ Textfield.view (Signal.forwardTo address AuthorYearOfBirthFieldAction) model.authorYearOfBirthField [] ]
    , cell
        [ size All 3 ]
        [ Button.raised
            (Signal.forwardTo address SubmitButtonAction)
            model.submitButton
            []
            [ text "Create book" ]
        ]
    ]
