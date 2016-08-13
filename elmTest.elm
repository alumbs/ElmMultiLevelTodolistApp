import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import String

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

-- test material
--type alias Todo = 
--  {id:Int}
--{id : Int, children : List Todo }
  
--testModel : Todo
--testModel = 
 --{
--   id = 0
 --}

type alias Todo =
    { id: Int
    , description: String
    , children: TodoChildren
    }

type TodoChildren = TodoChildren (List Todo)

--Create a new Todo Entry
newEntry: String -> Int -> Todo
newEntry str newId =
  {
      id = newId
    , description = str
    , children = (TodoChildren [])
  }

-- type TodoList = TodoList (List Todo)
--type List Todo = Empty | Node Todo (List Todo)
  
type alias Model =
 { entries : TodoChildren--List Todo
 , field: String
 , uid : Int
 }

emptyModel : Model
emptyModel = 
 {
   entries = (TodoChildren [])
   , field = ""
   , uid = 0
 }

init : (Model, Cmd Msg)
init =
  ( emptyModel, Cmd.none)


-- UPDATE

type Msg
  = NoOp
  | Add
  | UpdateTodo Int String
  | UpdateField String
  | AddChildTodo Int
  -- | NewFace Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
 case msg of
   NoOp ->
    model ! []

   Add -> 
    (addTodo model) ! [] --Model "new" 1)

   UpdateTodo id newDescription ->
    ((updateTodoModelDesc model id newDescription), Cmd.none )
    -- {model | entries = updateTodochild } ! []
    -- let
    --   updateTodo t =
    --     if t.id == id then {t | description = newDescription} else t
    -- in
    --   ({model | entries = List.map updateTodo model.entries }) ! []

   UpdateField str  ->
    { model | field = str } ! []

   AddChildTodo todoId ->
    ((createNewChildForTodo model todoId), Cmd.none)

    

--FUNCTIONS
createNewChildForTodo : Model -> Int -> Model
createNewChildForTodo model todoId =
  {
    model 
    | uid= model.uid + 1
    , entries = (addNewTodoToChildrenList model.entries todoId (model.uid + 1) )
  }
  --  let
  --   createChildTodo todo newTodoId =
  --     if todo.id == todoId then 
  --       { 
  --         todo 
  --         | children = 
  --           [{ id = newTodoId 
  --             , description = "new child"
  --             , children = (TodoChildren [])}]
  --       } 
  --     else todo
  --  in
  --   {model 
  --    | uid= model.uid + 1
  --    , entries = List.map createChildTodo model.entries model.uid++ 
  --   }

addNewTodoToChildrenList : TodoChildren -> Int -> Int -> TodoChildren
addNewTodoToChildrenList (TodoChildren todoChild) todoId newTodoId =
  let
    createChildTodo todo =
      if todo.id == todoId then 
        { 
          todo 
          | children = addTodoChildToChildrenList todo.children newTodoId
            -- TodoChildren (
            --   [
            --     { 
            --       id = newTodoId 
            --       , description = "new child"
            --       , children = (TodoChildren [])
            --     }
            --   ])
        } 
      else todo
   in
    TodoChildren (List.map createChildTodo todoChild)

addTodoChildToChildrenList : TodoChildren -> Int -> TodoChildren
addTodoChildToChildrenList (TodoChildren todo) newTodoId =
  -- {
  --   todo ++ 
  --   [
  --     { 
  --       id = newTodoId 
  --       , description = "new child"
  --       , children = (TodoChildren [])
  --     }
  --   ]
  -- }
  (TodoChildren (todo ++ [{children = (TodoChildren []), description = "", id = newTodoId}]))

updateTodoModelDesc : Model -> Int -> String -> Model
updateTodoModelDesc model id desc =
  {
    model
    | entries = (updateTodochildDesc model.entries id desc) 
  }


updateTodochildDesc : TodoChildren -> Int -> String -> TodoChildren
updateTodochildDesc (TodoChildren todoChild) todoId newDesc =
  let
    updateTodoDesc todo =
      if todo.id == todoId then {todo | description = newDesc} else todo
  in
    TodoChildren (List.map updateTodoDesc todoChild)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- Functions
-- getNextTodoId : Model -> Int
-- getNextTodoId model = 
--   (model.uid = model.uid + 1)

addTodo : Model -> Model
addTodo model =
   { model
    | uid = model.uid + 1
    , field = ""
    , entries =
      if String.isEmpty model.field then
        model.entries
      else
        --model.entries ++ [newEntry model.field model.uid]
        addToTodoChild model.entries model.field model.uid
   }      

addToTodoChild : TodoChildren -> String -> Int -> TodoChildren
addToTodoChild (TodoChildren todoChild) newTodoText newTodoVal =
  (TodoChildren (todoChild ++ [{children = (TodoChildren []), description = newTodoText, id = newTodoVal}]))

--Implement onEnter keyboard function
onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then msg else NoOp
  in
    on "keydown" (Json.map tagger keyCode)


-- VIEW FUNCTIONS

--show the root view
showRootView : String -> Html Msg
showRootView str = 
   div[]
   [
      h1 [] [ text "Welcome to Todolist" ]
    , h2 [] [ text "Below are the list of todos" ]
    , div [] [text str]
    , input 
      [
        placeholder "Enter the name of a new Todo"
      , value str
      , autofocus True
      , onInput UpdateField
      , onEnter Add
      ] 
      []
    , button [onClick Add] [text "Add New Todo"]
   ]

--Display a list of todos
displayTodoList : TodoChildren -> Html Msg
displayTodoList (TodoChildren todoList) =
  div[]
  (List.map displaySingleTodo todoList)

--Style
myStyle : Attribute Msg
myStyle =
 style[
   ("width", "100%")
 ]

margin15Style : Attribute Msg
margin15Style = 
  style[
    ("margin-left", "15px")
  ]

--Display single todo
displaySingleTodo : Todo -> Html Msg
displaySingleTodo todo =
  div[margin15Style] --style [("margin-left", "15px;")] ]
  [
    input[myStyle 
    -- , placeholder "New todo"
    , onEnter (AddChildTodo todo.id) --(AddChildTodo todo.children "" -1)
    , onInput (UpdateTodo todo.id)
    , value todo.description
    ]
    [
      -- text todo.description
    ]
    , div [ margin15Style] --style [("margin-left", "15px;")] ]
    [
      displayTodoList todo.children
    ]
  ]


-- displayChildTodoList : TodoList -> Html Msg
-- displayChildTodoList todoList =
--   div[]
--   (List.map displaySingleTodo todoList)

--Display single todo
-- displaySingleTodo : Todo -> Html Msg
-- displaySingleTodo todo =
--   div[]
--   [
--     input[myStyle 
--     -- , onTodoEnter (AddChildTodo todo)
--     , onInput (UpdateTodo todo.id)
--     , value todo.description
--     ]
--     [
--       -- text todo.description
--     ]
--     , div [ style ("margin-left", "15px;") ]
--     [
--       displayChildTodoList todo.children
--     ]
--   ]

-- VIEW
view : Model -> Html Msg
view model =
    div []
     [ showRootView model.field
     , displayTodoList model.entries
     ]