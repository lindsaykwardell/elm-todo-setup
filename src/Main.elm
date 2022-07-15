module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)


type alias Model =
    { todos : List Todo
    , newTodo : String
    , filter : TodoFilter
    }


type alias Todo =
    { id : Int
    , task : String
    , completed : Bool
    }


type Msg
    = UpdateNewTodoText String
    | ToggleTodoComplete Todo Bool
    | UpdateTodoText Todo String
    | AddTodo
    | DeleteTodo Todo
    | UpdateFilter TodoFilter


type TodoFilter
    = All
    | Active
    | Completed


main : Program () Model Msg
main =
    Browser.sandbox
        { init =
            { todos = []
            , newTodo = ""
            , filter = All
            }
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateNewTodoText text ->
            { model | newTodo = text }

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
            { model
                | todos =
                    model.todos
                        ++ [ { id = List.length model.todos + 1
                             , task = model.newTodo
                             , completed = False
                             }
                           ]
                , newTodo = ""
            }

        DeleteTodo todo ->
            { model | todos = List.filter (.id >> (/=) todo.id) model.todos }

        UpdateFilter filter ->
            { model | filter = filter }


view : Model -> Html Msg
view model =
    div [ style "width" "200px", style "margin" "auto" ]
        [ h1 [] [ text "Todos" ]
        , main_ []
            [ div []
                (model.todos
                    |> List.filterMap
                        (\todo ->
                            let
                                render =
                                    Just <|
                                        div [ style "display" "flex" ]
                                            [ input
                                                [ type_ "checkbox"
                                                , checked todo.completed
                                                , onCheck (ToggleTodoComplete todo)
                                                ]
                                                []
                                            , input
                                                [ style "flex-grow" "1"
                                                , value todo.task
                                                , onInput (UpdateTodoText todo)
                                                ]
                                                []
                                            , button [ onClick (DeleteTodo todo) ] [ text "X" ]
                                            ]
                            in
                            case model.filter of
                                All ->
                                    render

                                Active ->
                                    if not todo.completed then
                                        render

                                    else
                                        Nothing

                                Completed ->
                                    if todo.completed then
                                        render

                                    else
                                        Nothing
                        )
                )
            , Html.form
                [ onSubmit AddTodo ]
                [ input [ value model.newTodo, onInput UpdateNewTodoText ] []
                , button [] [ text "Add todo" ]
                ]
            , div [ style "display" "flex", style "justify-content" "space-between", style "padding-top" "15px" ]
                [ button [ activeFilterStyle model.filter All, onClick (UpdateFilter All) ] [ text "All" ]
                , button [ activeFilterStyle model.filter Active, onClick (UpdateFilter Active) ] [ text "Active" ]
                , button [ activeFilterStyle model.filter Completed, onClick (UpdateFilter Completed) ] [ text "Completed" ]
                ]
            ]
        ]


activeFilterStyle : TodoFilter -> TodoFilter -> Attribute msg
activeFilterStyle activeFilter filter =
    if activeFilter == filter then
        style "background" "lightblue"

    else
        style "background" "transparent"
