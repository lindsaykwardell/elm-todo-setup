module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)


type alias Model =
    { todos : List Todo
    }


type alias Todo =
    { id : Int
    , task : String
    , completed : Bool
    }


type Msg
    = ToggleTodoComplete Todo Bool
    | UpdateTodoText Todo String
    | AddTodo


main : Program () Model Msg
main =
    Browser.sandbox { init = { todos = [] }, update = update, view = view }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleTodoComplete todo completed ->
            { model
                | todos =
                    List.map
                        (\t ->
                            if t.id == todo.id then
                                { todo | completed = completed }

                            else
                                t
                        )
                        model.todos
            }

        UpdateTodoText todo task ->
            { model
                | todos =
                    List.map
                        (\t ->
                            if t.id == todo.id then
                                { todo | task = task }

                            else
                                t
                        )
                        model.todos
            }

        AddTodo ->
            { model | todos = model.todos ++ [ { id = List.length model.todos + 1, task = "", completed = False } ] }


view : Model -> Html Msg
view model =
    div [ style "width" "200px", style "margin" "auto"]
        [ h1 [] [ text "Todos" ]
        , main_ []
            [ div []
                (model.todos
                    |> List.map
                        (\todo ->
                            div [ style "display" "flex" ]
                                [ input [ type_ "checkbox", onCheck (ToggleTodoComplete todo) ] []
                                , input [ value todo.task, onInput (UpdateTodoText todo) ] []
                                ]
                        )
                )
            , button [ onClick AddTodo ] [ text "Add todo" ]
            ]
        ]
