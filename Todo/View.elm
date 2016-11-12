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
  in
    div
    [
      margin15Style 
      , marginHalfemTopStyle
      , attribute "id" (toString todo.id)
      , classList [("completed", todo.completed), ("todoParentContainer", True)]
    ]
    [ 
      div
      [ 
        class "todoDetailsContainer"
      ]
      [
        div
        [class "todoControls"]
        [
          button
          [
            onClick (ToggleShowChildTodos todo.children todo.id)
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
        , input
          [
            -- myStyle          
            class "edit"
            , placeholder "New todo"
            , id ("todo-" ++ toString todo.id)
            -- , onCtrlEnter (AddSiblingTodo todo.id)
            , onEnter (AddChildTodo todo)
            , onInput (UpdateTodo todo.id)
            , value todo.description
          ]
          [] 
      ] 
      , div 
        [ 
          margin15Style
          , class ( String.append "todoChildContainer " (showChildren todo.childrenVisible) )
        ] 
        [
          displayTodoList todo.children
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

-- VIEW
view : Model -> Html Msg
view model =
    div []
     [ 
       showRootView model.field
       , displayTodoList model.entries
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