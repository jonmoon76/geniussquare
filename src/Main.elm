module Main exposing (..)

import Browser
import Types exposing (..)
import Html exposing (Html, div)
import Piece


init : ( Model, Cmd Msg )
init = ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->  (model, Cmd.none )



view : Model -> Html Msg
view model =
    div [] []


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
