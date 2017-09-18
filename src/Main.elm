module Main exposing (main)

import Html exposing (Html, div, text, program, br, input, form, nav, a)
import Html.Attributes exposing (class, type_, placeholder, id, value, href)
import Html.Events exposing (onInput, onSubmit)
import Html.Attributes exposing (class)
import Http
import Date
import Json.Decode as Decode


-- USER DEFINED IMPORTS

import Utils


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



--Model


type alias Model =
    { currentInput : String
    , coordinates : Coordinates
    , currently : Prediction
    , predictions : List Prediction
    }


type alias Coordinates =
    { longitude : Float
    , latitude : Float
    }


type alias Prediction =
    { time : Date.Date
    , precipProbability : Float
    , summary : String
    }


type alias PredictionResponse =
    { currently : Prediction
    , predictions : List Prediction
    }


init : ( Model, Cmd Msg )
init =
    let
        coordinates =
            { longitude = -74.00597, latitude = 40.71427 }

        currentInput =
            "New York City, NY"

        currently =
            { time = Date.fromTime 0.0, precipProbability = 0.0, summary = "" }

        model =
            { currentInput = currentInput, coordinates = coordinates, currently = currently, predictions = [] }
    in
        ( model
        , Cmd.none
        )



--UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewInput input ->
            ( { model | currentInput = input }, Cmd.none )

        FetchPredictionResult ->
            ( model, fetchPredictionResult )

        PredictionResult (Ok result) ->
            ( { model
                | currently = result.currently
                , predictions = result.predictions
              }
            , Cmd.none
            )

        PredictionResult (Err error) ->
            let
                _ =
                    Debug.log "Error" error
            in
                ( model, Cmd.none )


fetchPredictionResult : Cmd Msg
fetchPredictionResult =
    let
        url =
            Utils.format strCoords "/api/predict?longitude=%s&latitude=%s"

        strCoords =
            List.map toString [ -74.00597, 40.71427 ]

        request =
            Http.get url responseDecoder

        responseDecoder =
            Decode.map2 PredictionResponse
                (Decode.field "currently" predictionDecoder)
                (Decode.at [ "hourly", "data" ] <| Decode.list predictionDecoder)
    in
        Http.send PredictionResult request


predictionDecoder : Decode.Decoder Prediction
predictionDecoder =
    let
        dateDecoder =
            Decode.float |> Decode.andThen convert

        convert =
            Decode.succeed << Date.fromTime
    in
        Decode.map3 Prediction
            (Decode.field "time" dateDecoder)
            (Decode.field "precipProbability" Decode.float)
            (Decode.field "summary" Decode.string)



-- MESSAGES


type Msg
    = NoOp
    | NewInput String
    | FetchPredictionResult
    | PredictionResult
        (Result Http.Error
            { currently : Prediction
            , predictions : List Prediction
            }
        )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ navbar
        , br [] []
        , searchInput model.currentInput
        , br [] []
        ]


searchInput : String -> Html Msg
searchInput val =
    let
        inputField =
            input
                [ class "form-control"
                , type_ "text"
                , placeholder "Type a name of city to get rain PredictionResult"
                , id "search"
                , onInput NewInput
                , value val
                ]
                []
    in
        form [ onSubmit FetchPredictionResult ] [ div [ class "form-group" ] [ inputField ] ]


navbar : Html Msg
navbar =
    let
        navclass =
            class "navbar navbar-expand-sm navbar-dark bg-dark"

        navlink link =
            a [ class "nav-link", href "#" ] [ text link ]
    in
        nav [ navclass ] [ div [ class "navbar-nav" ] [ navlink "Home" ] ]
