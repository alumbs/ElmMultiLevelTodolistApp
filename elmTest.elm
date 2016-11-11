import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import String
import Array

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Todo =
    { id: Int
    , description: String
    , children: TodoChildren
    }

type TodoChildren = TodoChildren (List Todo)

type AllTodoChildren = AllTodoChildren (Array.Array Todo)

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
  | DeleteTodo Int
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

   UpdateField str  ->
    { model | field = str } ! []

   AddChildTodo todoId ->
    ((createNewChildForTodo model todoId), Cmd.none)

   DeleteTodo todoId ->
    ((deleteTodoFromModel model todoId), Cmd.none)


--FUNCTIONS
createNewChildForTodo : Model -> Int -> Model
createNewChildForTodo model todoId =
  {
    model 
    | uid= model.uid + 1
    , entries = (addNewTodoToChildrenList model.entries todoId (model.uid) )
  }
  

addNewTodoToChildrenList : TodoChildren -> Int -> Int -> TodoChildren
addNewTodoToChildrenList (TodoChildren todoChild) parentTodoId newTodoId =
  let
    createChildTodo todo =
      if todo.id == parentTodoId then 
        { 
          todo 
          | children = addEmptyTodoChildToChildrenList todo.children newTodoId            
        } 
      else 
        recursiveAddNewTodoItem todo parentTodoId newTodoId
   in
    TodoChildren (List.map createChildTodo todoChild)


recursiveAddNewTodoItem : Todo -> Int -> Int -> Todo
recursiveAddNewTodoItem todo parentTodoId newTodoId =
  {
    todo
    | children = addNewTodoToChildrenList todo.children parentTodoId newTodoId
  }

addEmptyTodoChildToChildrenList : TodoChildren -> Int -> TodoChildren
addEmptyTodoChildToChildrenList (TodoChildren todo) newTodoId =
  (TodoChildren (todo ++ [(newEntry "" newTodoId)]))

updateTodoModelDesc : Model -> Int -> String -> Model
updateTodoModelDesc model id desc =
  {
    model
    | entries =  (updateTodochildDesc model.entries id desc) 
  }

updateTodoItemDescription : Todo -> String -> Todo
updateTodoItemDescription todo newDesc =
  {
    todo
    | description = newDesc
  }

{-- 
  This method is used to update the description of the todos
--}
updateTodochildDesc : TodoChildren -> Int -> String -> TodoChildren
updateTodochildDesc (TodoChildren todoChild) todoId newDesc =
  let
    updateTodoDesc todo =
      if todo.id == todoId then 
        updateTodoItemDescription todo newDesc 
      else 
        -- todo
        recursiveUpdateSingleTodoDesc todo todoId newDesc
        
  in
    TodoChildren (List.map updateTodoDesc todoChild)

recursiveUpdateSingleTodoDesc : Todo -> Int -> String -> Todo
recursiveUpdateSingleTodoDesc todoItem todoId newDesc =  
  {
    todoItem
    | children = updateTodochildDesc todoItem.children todoId newDesc
  }  


-- Delete todo from model
deleteTodoFromModel : Model -> Int -> Model
deleteTodoFromModel model todoToDeleteId =
  {
    model
    | entries =  (deleteTodoFromList model.entries todoToDeleteId) 
  }

{-- 
  This method is used to delete todos
--}
deleteTodoFromList : TodoChildren -> Int -> TodoChildren
deleteTodoFromList (TodoChildren todoChild) todoToDeleteId =
  let
    removeTodo todo =
      if todo.id == todoToDeleteId then
        Nothing
      else
        Just (recursiveDeleteTodo todo todoToDeleteId)      
  in
    (TodoChildren (List.filterMap removeTodo todoChild))

recursiveDeleteTodo : Todo -> Int -> Todo
recursiveDeleteTodo todo todoToDeleteId =
  {
    todo
    | children = (deleteTodoFromList todo.children todoToDeleteId)
  }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


addTodo : Model -> Model
addTodo model =
  model
    |> addNewTodoItemToModel
    |> updateModelUniversalId
    |> resetModelStringField   

resetModelStringField : Model -> Model
resetModelStringField model =
   { 
     model
     | field = ""
   }

updateModelUniversalId : Model -> Model
updateModelUniversalId model =
   { 
     model
     | uid = model.uid + 1    
   }

getLastTodoFromList : TodoChildren -> Todo
getLastTodoFromList (TodoChildren todolist) =
  case List.head (List.reverse todolist) of
    Nothing ->
      (newEntry "" 1 )
    Just val ->
      val
  

addNewTodoItemToModel : Model -> Model
addNewTodoItemToModel model =
  { model
    | entries =
      if String.isEmpty model.field then
        model.entries
      else
        addToTodoChild model.entries model.field (model.uid)
   }

addToTodoChild : TodoChildren -> String -> Int -> TodoChildren
addToTodoChild (TodoChildren todoChild) newTodoText newTodoVal =
  (TodoChildren (todoChild ++ [(newEntry newTodoText newTodoVal)]))

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
   div[margin1emBotStyle]
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

--Display single todo
displaySingleTodo : Todo -> Html Msg
displaySingleTodo todo =
  div[margin15Style , attribute "id" (toString todo.id) ] --style [("margin-left", "15px;")] ]
  [
    button
    [
      onClick (DeleteTodo todo.id) 
    ]
    [
      text "delete"
    ]
    , input
    [
      myStyle 
      , placeholder "New todo"
      , onEnter (AddChildTodo todo.id)
      , onInput (UpdateTodo todo.id)
      , value todo.description
    ][]
    , div [ margin15Style] --style [("margin-left", "15px;")] ]
    [
      displayTodoList todo.children
    ]
  ]

-- VIEW
view : Model -> Html Msg
view model =
    div []
     [ 
       showRootView model.field
       , displayTodoList model.entries
     ]


--STLYES
myStyle : Attribute Msg
myStyle =
 style[
   ("width", "80%")
   , ("margin-left", "15px")
 ]

margin15Style : Attribute Msg
margin15Style = 
  style[
    ("margin-left", "15px")
  ]

margin1emBotStyle : Attribute Msg
margin1emBotStyle = 
  style[
    ("margin-bottom", "1em")
  ]

margin2emTopStyle : Attribute Msg
margin2emTopStyle = 
  style[
    ("margin-top", "2em")
  ]