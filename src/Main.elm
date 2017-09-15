module Main exposing (..)

import Html exposing (Html, div, text, program)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--Model


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( "Hello, from elm!", Cmd.none )



--MESSAGES


type Msg
    = NoOp



--UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ text model ]
