module BooksList.Types (..) where

import Material.Button as Button

import Generated.Api exposing (Book)

type alias Model =
  { books : List Book
  , refreshButton : Button.Model
  }


type Action
  = SetBooks (Maybe (List Book))
  | RefreshButtonAction Button.Action
