module Types (..) where

import Material.Layout as Layout

import BooksList.Types
import NewBookForm.Types


type alias Model =
  { booksList : BooksList.Types.Model
  , newBookForm : NewBookForm.Types.Model
  , mdlLayout : Layout.Model
  }


type Action
  = BooksListAction BooksList.Types.Action
  | NewBookFormAction NewBookForm.Types.Action
  | MDLLayout Layout.Action
