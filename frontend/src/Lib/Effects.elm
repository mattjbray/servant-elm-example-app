module Lib.Effects (andThen, pure) where

import Effects exposing (Effects)


pure : a -> ( a, Effects b )
pure model =
  ( model, Effects.none )


andThen : ( a, Effects e ) -> (a -> ( b, Effects e )) -> ( b, Effects e )
andThen ( model, effects ) f =
  let
    ( newModel, fx ) =
      f model
  in
    ( newModel
    , Effects.batch
        [ effects
        , fx
        ]
    )
