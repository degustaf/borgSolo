-- Copyright (c) 2026 Derek Gustafson.


module Main exposing (main)

import Browser
import Html exposing (Html, div)


type Model
    = Nothing


view : Model -> Html msg
view _ =
    div [] []


update : msg -> Model -> ( Model, Cmd msg )
update _ model =
    ( model, Cmd.none )


init : () -> ( Model, Cmd msg )
init _ =
    ( Nothing, Cmd.none )


main : Program () Model msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
