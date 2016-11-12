module Todo.Subscriptions exposing (subscriptions)

import Keyboard exposing (..)
import Todo.Types exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Sub.none
    Sub.batch
    [ 
        Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
    ]