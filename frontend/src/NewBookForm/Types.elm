module NewBookForm.Types (..) where

import Material.Button as Button
import Material.Textfield as Textfield


type alias Model =
  { title : Maybe String
  , titleField : Textfield.Model
  , authorName : Maybe String
  , authorNameField : Textfield.Model
  , authorYearOfBirth : Maybe Int
  , authorYearOfBirthField : Textfield.Model
  , submitButton : Button.Model
  }


type Action
  = CreateBook
  | FetchBooks
  | TitleFieldAction Textfield.Action
  | AuthorNameFieldAction Textfield.Action
  | AuthorYearOfBirthFieldAction Textfield.Action
  | SubmitButtonAction Button.Action
