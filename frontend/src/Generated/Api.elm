module Generated.Api where

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (apply)
import Http
import String
import Task


getBooks : Task.Task Http.Error List Book
getBooks =
  let request =
        { verb = "GET"
        , headers = [("Content-Type", "application/json")]
        , url = "http://localhost:8000/api/books"
        , body = Http.empty
        }
  in  Http.fromJson
        list decodeBook
        (Http.send Http.defaultSettings request)