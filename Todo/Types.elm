module Todo.Types exposing (..)

import Keyboard exposing (..)
import String

type alias Todo =
    { id: Int
    , description: String
    , completed: Bool
    , childrenVisible: Bool
    , children: TodoChildren
    }

type TodoChildren = TodoChildren (List Todo)

type alias Model =
 { 
   entries : TodoChildren--List Todo
   , field: String
   , uid : Int
--    , keysPressed : KeyDowns
 }

type Msg
  = NoOp
  | Add
  | UpdateTodo Int String
  | UpdateField String
  | AddChildTodo Int
  | DeleteTodo Int
  | ToggleTodoCompleted Int
  | ToggleShowChildTodos TodoChildren Int
  