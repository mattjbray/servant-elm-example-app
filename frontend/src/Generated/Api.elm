module Generated.Api where

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (apply)
import Http
import String
import Task


type alias Book =
  {bookId : Maybe String
  ,title : String
  ,author : Author}

type alias Author =
  {name : String
  ,yearOfBirth : Int}

decodeBook : Decoder Book
decodeBook = Book
  `map`   ("bookId" := maybe string)
  `apply` ("title" := string)
  `apply` ("author" := decodeAuthor)

decodeAuthor : Decoder Author
decodeAuthor = Author
  `map`   ("name" := string)
  `apply` ("yearOfBirth" := int)

getBooks : Task.Task Http.Error (List (Book))
getBooks =
  let request =
        { verb = "GET"
        , headers = [("Content-Type", "application/json")]
        , url = "http://localhost:8000/api/books"
        , body = Http.empty
        }
  in  Http.fromJson
        (list decodeBook)
        (Http.send Http.defaultSettings request)

postBooks : Book -> Task.Task Http.Error (Book)
postBooks body =
  let request =
        { verb = "POST"
        , headers = [("Content-Type", "application/json")]
        , url = "http://localhost:8000/api/books"
        , body = Http.empty
        }
  in  Http.fromJson
        (decodeBook)
        (Http.send Http.defaultSettings request)