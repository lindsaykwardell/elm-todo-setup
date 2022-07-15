module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { todos : List Todo
    }


type alias Todo =
    { task : String
    }


type Msg
    = NoOp


main : Program () Model Msg
main =
    Browser.sandbox { init = { todos = [] }, update = update, view = view }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Todos" ] ]
