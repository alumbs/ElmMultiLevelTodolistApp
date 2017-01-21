module Todo.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Todo.Types exposing (..)
import String
import Json.Decode as Json

-- VIEW FUNCTIONS
onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then msg else NoOp
  in
    on "keydown" (Json.map tagger keyCode)

--show the root view
showRootView : String -> Int -> Html Msg
showRootView str selectedTodoId= 
  let
    isSpecificTodoSelected =
      if selectedTodoId /= -1 then
        True
      else
        False
  in
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
      , button 
        [
          margin15Style
          , onClick GotoHome
          , classList
            [
              ("specificTodoSelected", isSpecificTodoSelected)
            ] 
        ] 
        [
          text "Home"
        ]
    ]

--Display a list of todos
displayTodoList : TodoChildren -> Int -> Html Msg
displayTodoList (TodoChildren todoList) showTodoId =
  let
    showThisTodo todo =
      displaySingleTodo todo showTodoId
  in
    div[]
    (List.map showThisTodo todoList)

--Display single todo
displaySingleTodo : Todo -> Int -> Html Msg
displaySingleTodo todo showTodoId =
  let
    showChildren areChildrenVisible =
      if areChildrenVisible then
        ""
      else
        "hideChildren"

    toggleMinimizeText childrenAreVisible =
       if childrenAreVisible then
          "-"
       else
          "+"

    showThisTodo thisTodoId showTodoId =
      if thisTodoId == showTodoId then
        True
      else
        False

    showAllTodos showTodoId =
      if showTodoId == -1 then
        True
      else
        False
  in
    div
    [
    --   margin15Style 
      marginHalfemTopStyle
      , attribute "id" (toString todo.id)
      , classList 
        [
          ("completed", todo.completed)
          , ("todoParentContainer", True)
          , ("showTodoParentContainer", showThisTodo todo.id showTodoId)
          , ("showAllTodos", showAllTodos showTodoId) 
        ]
    ]
    [ 
      div
      [ 
        classList 
        [
          ("todoDetailsContainer", True)
          , ("showTodoDetailsContainer", showThisTodo todo.id showTodoId)
        ]
      ]
      [
        div
        [class "todoControls"]
        [
          button
          [
            onClick (ViewThisTodo todo.id)
            , title "View Todo"
          ]
          [ text "View"]
          , button
          [
            margin15Style
            , onClick (ToggleShowChildTodos todo.children todo.id)
            , title "Minimize"
          ]
          [
            text (toggleMinimizeText todo.childrenVisible)
          ]
          , button
            [
              margin15Style
              , onClick (DeleteTodo todo.id) 
              , title "Delete"
            ]
            [
              text "X"
            ]
          , input
            [ 
              margin15Style
              , type' "checkbox"
              , checked (todo.completed )
              , onClick (ToggleTodoCompleted todo.id) 
            ]
            [] 
        ]
        , div
          [
            -- myStyle          
            class "edit"
            -- , placeholder "New todo"
            , id ("todo-" ++ toString todo.id)
            -- , onCtrlEnter (AddSiblingTodo todo.id)
            , onEnter (AddChildTodo todo)
            , onInput (UpdateTodo todo.id)
            , value todo.description
            , contenteditable True
          ]
          [
            text todo.description
          ] 
      ] 
      , div 
        [ 
        --   margin15Style
          class ( String.append "todoChildContainer " (showChildren todo.childrenVisible) )
        ] 
        [
          displayTodoList todo.children showTodoId
        ]
    ]

displayFooter : Html Msg
displayFooter = 
   footer
   []
   [
        section
        []
        [
            header
            []
            [
                text "Some tips are shown below"
            ]
            ,   ul
                []
                [
                    li
                    []
                    [
                        text "press Enter to create a new todo on the next level"
                    ]
                    ,   li
                        []
                        [
                            text "press Ctrl+Enter to create a new todo on the same level"
                        ]
                ]
        ]
        

   ]

showTodoOrHome : Int -> TodoChildren -> Html Msg
showTodoOrHome singleTodoId todoList =
  displayTodoList todoList singleTodoId

-- VIEW
view : Model -> Html Msg
view model =
    div []
     [ 
       showRootView model.field model.selectedTodoId
       , showTodoOrHome model.selectedTodoId model.entries
       , displayFooter
     ]


--STLYES
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

marginHalfemTopStyle : Attribute Msg
marginHalfemTopStyle = 
  style[
    ("margin-top", "0.5em")
  ]