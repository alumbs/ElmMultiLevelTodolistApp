module Todo.State exposing (init, update)

import Todo.Types exposing (..)
-- import Todo.Key exposing (..)
-- import Todo.Functions exposing (..)
import String
import Keyboard exposing (..)
-- import Set exposing (Set)


init : (Model, Cmd Msg)
init =
  ( emptyModel, Cmd.none)


emptyModel : Model
emptyModel = 
 {
   entries = (TodoChildren [])
   , field = ""
   , uid = 0
--    , keysPressed = Set.empty
 }

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

   ToggleTodoCompleted todoId ->
    ((toggleTodoCompletedField model todoId) , Cmd.none)

   ToggleShowChildTodos todolist todoId ->
    if todolist == (TodoChildren []) then
      model ! []
    else
      ((toggleShowChildrenVisibleField model todoId) , Cmd.none)



newEntry: String -> Int -> Todo
newEntry str newId =
  {
      id = newId
    , description = str
    , completed = False
    , childrenVisible = True
    , children = (TodoChildren [])
  }

toggleShowChildrenVisibleField : Model -> Int -> Model
toggleShowChildrenVisibleField model todoId =
  {
    model
    | entries = (toggleChildrenVisibleField model.entries todoId)
  }

toggleChildrenVisibleField : TodoChildren -> Int -> TodoChildren
toggleChildrenVisibleField (TodoChildren todolist) todoId =
  let
    toggleChildrenVisible todo =
      if todo.id == todoId then
        updateChildrenVisibleField todo
      else
        recursiveUpdateChildrenVisibleField todo todoId
  in
    (TodoChildren (List.map toggleChildrenVisible todolist))  

recursiveUpdateChildrenVisibleField : Todo -> Int -> Todo
recursiveUpdateChildrenVisibleField todo todoId =
  {
    todo
    | children = (toggleChildrenVisibleField todo.children todoId)
  }


updateChildrenVisibleField : Todo -> Todo
updateChildrenVisibleField todo =
  {
    todo
    | childrenVisible = not todo.childrenVisible
  }

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

--recursively delete todos
recursiveDeleteTodo : Todo -> Int -> Todo
recursiveDeleteTodo todo todoToDeleteId =
  {
    todo
    | children = (deleteTodoFromList todo.children todoToDeleteId)
  }


toggleTodoCompletedField : Model -> Int -> Model
toggleTodoCompletedField model todoCompletedId =
  {
    model
    | entries = (toggleCompletedTodoStatus model.entries todoCompletedId)
  }

toggleCompletedTodoStatus : TodoChildren -> Int -> TodoChildren
toggleCompletedTodoStatus (TodoChildren todolist) completedTodoId =
  let
    markTodoAsComplete todo =
      if todo.id == completedTodoId then
        toggleSetTodoComplete todo (not todo.completed)
      else
        recursiveFindCompletedTodo todo completedTodoId
  in
    (TodoChildren (List.map markTodoAsComplete todolist))


recursiveFindCompletedTodo : Todo -> Int -> Todo
recursiveFindCompletedTodo todo completedTodoId =
  {
    todo
    | children = toggleCompletedTodoStatus todo.children completedTodoId  
  }

toggleSetTodoComplete : Todo -> Bool -> Todo
toggleSetTodoComplete todo isTodoCompleted =
  {
    todo
    | completed = isTodoCompleted
    , children = recursiveSetTodoChildrenToComplete todo.children isTodoCompleted
  }

recursiveSetTodoChildrenToComplete : TodoChildren -> Bool -> TodoChildren
recursiveSetTodoChildrenToComplete (TodoChildren todolist) isTodoCompleted =
  let
    markTodoAsComplete todo =
      toggleSetTodoComplete todo isTodoCompleted  
  in
    (TodoChildren (List.map markTodoAsComplete todolist))


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