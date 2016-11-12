port module Todo exposing (..)

import Html exposing (..)
import Html.App as App
import Todo.State exposing (..)
import Todo.View exposing (..)
import Todo.Subscriptions exposing (subscriptions)

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }