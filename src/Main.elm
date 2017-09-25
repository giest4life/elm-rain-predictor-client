module Main exposing (main)

import Html exposing (Html, div, text, program, br, input, form, nav, a, table, thead, tbody, td, tr, th, datalist, option)
import Html.Attributes exposing (class, type_, placeholder, id, value, href, autocomplete, autofocus, list)
import Html.Events exposing (onInput, onSubmit, onClick)
import Html.Attributes exposing (class)
import Http
import Date
import Date.Format as DF
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
    , inputSelected : Bool
    , selectedLocation : Maybe Location
    , suggestedLocations : List Location
    , highlightedLocation : Maybe Location
    , showSuggestions : Bool
    , autocompleted : Bool
    , showPredictions : Bool
    , currently : Maybe Prediction
    , predictions : List Prediction
    }


type alias Prediction =
    { time : Date.Date
    , precipProbability : Float
    , summary : String
    }


type alias Location =
    { geonameId : Float
    , locationName : String
    , longitude : Float
    , latitude : Float
    , countryCode : String
    , countryName : String
    , admin1Code : String
    , admin1Name : String
    }


type alias PredictionResponse =
    { currently : Prediction
    , predictions : List Prediction
    }


init : ( Model, Cmd Msg )
init =
    let
        selectedLocation =
            Location 5128581 "New York City" -74.00597 40.71427 "US" "United States of America" "NY" "New York"

        currentInput =
            "New York City, NY"

        model =
            Model currentInput False (Just selectedLocation) [] Nothing False False False Nothing []
    in
        ( model
        , fetchPredictionResult model.selectedLocation
        )



--UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewInput input ->
            handleSearchInput model input

        FetchPredictionResult location ->
            ( model, fetchPredictionResult location )

        PredictionResult (Ok result) ->
            ( { model
                | currently = Just result.currently
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

        LocationQuery query ->
            ( model, fetchLocationResult query )

        LocationSelect location ->
            ( { model
                | currentInput = formatLocationResult location
                , showSuggestions = False
              }
            , handleLocationSelect (Just location)
            )

        LocationResult (Ok locations) ->
            ( { model | suggestedLocations = locations }, Cmd.none )

        LocationResult (Err error) ->
            let
                _ =
                    Debug.log "Error" error
            in
                ( model, Cmd.none )


handleLocationSelect : Maybe Location -> Cmd Msg
handleLocationSelect =
    fetchPredictionResult


fetchPredictionResult : Maybe Location -> Cmd Msg
fetchPredictionResult l =
    case l of
        Nothing ->
            Cmd.none

        Just location ->
            let
                url =
                    Utils.format strCoords "/api/predict?longitude=%s&latitude=%s"

                strCoords =
                    List.map toString [ location.longitude, location.latitude ]

                request =
                    Http.get url responseDecoder

                responseDecoder =
                    Decode.map2 PredictionResponse
                        (Decode.field "currently" predictionDecoder)
                        (Decode.at [ "hourly", "data" ] <| Decode.list predictionDecoder)
            in
                Http.send PredictionResult request


fetchLocationResult : String -> Cmd Msg
fetchLocationResult query =
    let
        url =
            Utils.format [ query ] "api/locations?query=%s"

        request =
            Http.get url responseDecoder

        responseDecoder =
            Decode.list locationDecoder
    in
        Http.send LocationResult request



-- INPUT KEYBOARD EVENTS


handleSearchInput : Model -> String -> ( Model, Cmd Msg )
handleSearchInput model input =
    if String.length input > 3 then
        ( { model | currentInput = input, showSuggestions = True }, fetchLocationResult input )
    else
        ( { model | currentInput = input, showSuggestions = False }, Cmd.none )



-- DECODERS


predictionDecoder : Decode.Decoder Prediction
predictionDecoder =
    let
        dateDecoder =
            Decode.float |> Decode.andThen convert

        convert =
            Decode.succeed << Date.fromTime << (*) 1000
    in
        Decode.map3 Prediction
            (Decode.field "time" dateDecoder)
            (Decode.field "precipProbability" Decode.float)
            (Decode.field "summary" Decode.string)


locationDecoder : Decode.Decoder Location
locationDecoder =
    Decode.map8 Location
        (Decode.field "geonameId" Decode.float)
        (Decode.field "locationName" Decode.string)
        (Decode.field "longitude" Decode.float)
        (Decode.field "latitude" Decode.float)
        (Decode.field "countryCode" Decode.string)
        (Decode.field "countryName" Decode.string)
        (Decode.field "admin1Code" Decode.string)
        (Decode.field "admin1Name" Decode.string)



-- MESSAGES


type Msg
    = NoOp
    | NewInput String
    | LocationQuery String
    | LocationSelect Location
    | LocationResult (Result Http.Error (List Location))
    | FetchPredictionResult (Maybe Location)
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
        , searchInput model.currentInput model.suggestedLocations
        , showLocationResults model.suggestedLocations
        , showPredictions model.currently model.predictions
        ]


searchInput : String -> List Location -> Html Msg
searchInput val locations =
    let
        inputField =
            input
                [ class "form-control"
                , type_ "text"
                , placeholder "Name of a city"
                , id "search"
                , onInput NewInput
                , value val
                , autofocus True
                , list "locations"
                ]
                []
    in
        form [ onSubmit << LocationQuery <| val ] [ div [ class "form-group" ] [ inputField ] ]


navbar : Html Msg
navbar =
    let
        navclass =
            class "navbar navbar-expand-sm navbar-dark bg-dark"

        navlink link =
            a [ class "nav-link", href "#" ] [ text link ]
    in
        nav [ navclass ] [ div [ class "navbar-nav" ] [ navlink "Home" ] ]


showLocationResults : List Location -> Html Msg
showLocationResults locations =
    let
        locationCard l =
            div [ class "card border border-secondary", onClick (LocationSelect l) ]
                [ div [ class "card-body" ] [ text << formatLocationResult <| l ] ]
    in
        case locations of
            [] ->
                div [] []

            _ ->
                div [] << List.map locationCard <| locations


predictionsTable : List Prediction -> Html Msg
predictionsTable preds =
    let
        header =
            thead [ class "thead-inverse" ] [ headerRow ]

        headerRow =
            tr []
                [ th [] [ text "Time" ]
                , th [] [ text "Conditions" ]
                , th [] [ text "Probability" ]
                ]

        tableBody =
            tbody [] <| List.map predToRow preds

        predToRow pred =
            tr []
                [ td [] [ text << DF.format "%A" <| pred.time ]
                , td [] [ text pred.summary ]
                , td [] [ text << toString <| pred.precipProbability ]
                ]
    in
        table [ class "table" ] [ header, tableBody ]



-- No need to show current if no other predictions available


showPredictions : Maybe Prediction -> List Prediction -> Html Msg
showPredictions current preds =
    case current of
        Nothing ->
            div [] []

        Just c ->
            case preds of
                [] ->
                    div [] []

                _ ->
                    predictionsTable (c :: preds)


formatLocationResult : Location -> String
formatLocationResult location =
    location.locationName ++ ", " ++ location.admin1Name
