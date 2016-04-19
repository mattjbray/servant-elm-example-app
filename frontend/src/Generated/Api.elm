module Generated.Api where

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import String
import Task


type alias Book =
  { bookId : Maybe String
  , title : String
  , author : Author
  , rating : Int
  }

type alias Author =
  { name : String
  , yearOfBirth : Int
  }

decodeBook : Json.Decode.Decoder Book
decodeBook =
  Json.Decode.succeed Book
    |: ("bookId" := Json.Decode.maybe Json.Decode.string)
    |: ("title" := Json.Decode.string)
    |: ("author" := decodeAuthor)
    |: ("rating" := Json.Decode.int)

decodeAuthor : Json.Decode.Decoder Author
decodeAuthor =
  Json.Decode.succeed Author
    |: ("name" := Json.Decode.string)
    |: ("yearOfBirth" := Json.Decode.int)

encodeBook : Book -> Json.Encode.Value
encodeBook x =
  Json.Encode.object
    [ ( "bookId", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.bookId )
    , ( "title", Json.Encode.string x.title )
    , ( "author", encodeAuthor x.author )
    , ( "rating", Json.Encode.int x.rating )
    ]

encodeAuthor : Author -> Json.Encode.Value
encodeAuthor x =
  Json.Encode.object
    [ ( "name", Json.Encode.string x.name )
    , ( "yearOfBirth", Json.Encode.int x.yearOfBirth )
    ]

getBooks : Task.Task Http.Error (List (Book))
getBooks =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "http://localhost:8000/api"
          ++ "/" ++ "books"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeBook)
      (Http.send Http.defaultSettings request)

postBooks : Book -> Task.Task Http.Error (Book)
postBooks body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "http://localhost:8000/api"
          ++ "/" ++ "books"
      , body =
          Http.string (Json.Encode.encode 0 (encodeBook body))
      }
  in
    Http.fromJson
      decodeBook
      (Http.send Http.defaultSettings request)