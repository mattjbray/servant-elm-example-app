module Generated.Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias Author =
    { name : String
    , yearOfBirth : Int
    }

decodeAuthor : Decoder Author
decodeAuthor =
    decode Author
        |> required "name" string
        |> required "yearOfBirth" int

encodeAuthor : Author -> Json.Encode.Value
encodeAuthor x =
    Json.Encode.object
        [ ( "name", Json.Encode.string x.name )
        , ( "yearOfBirth", Json.Encode.int x.yearOfBirth )
        ]

type alias Book =
    { bookId : Maybe (String)
    , title : String
    , author : Author
    }

decodeBook : Decoder Book
decodeBook =
    decode Book
        |> required "bookId" (maybe string)
        |> required "title" string
        |> required "author" decodeAuthor

encodeBook : Book -> Json.Encode.Value
encodeBook x =
    Json.Encode.object
        [ ( "bookId", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.bookId )
        , ( "title", Json.Encode.string x.title )
        , ( "author", encodeAuthor x.author )
        ]

getBooks : Http.Request (List (Book))
getBooks =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8000/api"
                , "books"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeBook)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postBooks : Book -> Http.Request (Book)
postBooks body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8000/api"
                , "books"
                ]
        , body =
            Http.jsonBody (encodeBook body)
        , expect =
            Http.expectJson decodeBook
        , timeout =
            Nothing
        , withCredentials =
            False
        }