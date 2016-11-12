module Todo.Types exposing (..)

import Keyboard exposing (..)
import String

type alias Todo =
    { id: Int
    , description: String
    , completed: Bool
    , childrenVisible: Bool
    , children: TodoChildren
    , parentId : Int
    }

type TodoChildren = TodoChildren (List Todo)

type alias Model =
 { 
   entries : TodoChildren--List Todo
   , field: String
   , uid : Int
   , keysDown : List Keyboard.KeyCode
 }

type Msg
  = NoOp
  | Add
  | UpdateTodo Int String
  | UpdateField String
  | AddChildTodo Todo
  | DeleteTodo Int
  | ToggleTodoCompleted Int
  | ToggleShowChildTodos TodoChildren Int
  | KeyDown KeyCode
  | KeyUp KeyCode
