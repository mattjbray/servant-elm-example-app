module Main (..) where

import Effects
import Html
import Material.Layout as Layout
import StartApp
import Task

import Types
import State
import View


app : StartApp.App Types.Model
app =
  StartApp.start
    { init = State.init
    , view = View.view
    , update = State.update
    , inputs =
        [ Layout.setupSignals Types.MDLLayout ]
    }


main : Signal Html.Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
