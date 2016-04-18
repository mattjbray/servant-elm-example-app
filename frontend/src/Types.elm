module Types (..) where

import Material.Button as Button
import Material.Layout as Layout

import Generated.Api exposing (Book)
import NewBookForm.Types

type alias Model =
  { books : List Book
  , newBookForm : NewBookForm.Types.Model
  , mdlLayout : Layout.Model
  , refreshButton : Button.Model
  }


type Action
  = SetBooks (Maybe (List Book))
  | NewBookFormAction NewBookForm.Types.Action
  | MDLLayout Layout.Action
  | RefreshButtonAction Button.Action
