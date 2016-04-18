module Lib (pure) where

import Effects exposing (Effects)


pure : a -> ( a, Effects b )
pure model =
  ( model, Effects.none )
