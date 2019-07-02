module Main exposing (Model, Msg(..), init, main, update, view, viewInput)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D
import Platform.Cmd
import Url.Builder as Url


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { startDate : String
    , initialBalance : Int
    , portfolioAllocation : Dict Int Stock
    , stockPerformance : Dict String StockPerformance
    }


init : () -> ( Model, Cmd msg )
init _ =
    let
        model =
            Model "" 0 (Dict.fromList [ ( 1, Stock "" 0 ) ]) Dict.empty
    in
    ( model, Cmd.none )


type alias StockPerformance =
    { name : String
    , price : Int
    , date : String
    }


type alias Stock =
    { name : String
    , allocation : Int
    }


updateStockAllocation : Maybe Int -> Maybe Stock -> Maybe Stock
updateStockAllocation allocation value =
    let
        newAllocation =
            Maybe.withDefault 0 allocation
    in
    case value of
        Nothing ->
            Just (Stock "" newAllocation)

        Just i ->
            Just { i | allocation = newAllocation }


updateStockName : String -> Maybe Stock -> Maybe Stock
updateStockName name value =
    case value of
        Nothing ->
            Just (Stock name 0)

        Just i ->
            Just { i | name = name }


type Msg
    = StartDate String
    | InitialBalance String
    | PortfolioAllocation Int String
    | PortfolioAllocationName Int String
    | AddStock
    | RemoveStock Int
    | SubmittedForm
    | GotStockData (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartDate startDate ->
            ( { model | startDate = startDate }, Cmd.none )

        InitialBalance input ->
            let
                initialBalance =
                    Maybe.withDefault 0 (String.toInt input)
            in
            ( { model | initialBalance = initialBalance }, Cmd.none )

        PortfolioAllocation id input ->
            let
                allocation =
                    String.toInt input

                portAllocation =
                    Dict.update id (updateStockAllocation allocation) model.portfolioAllocation
            in
            ( { model | portfolioAllocation = portAllocation }, Cmd.none )

        PortfolioAllocationName id input ->
            let
                portAllocation =
                    Dict.update id (updateStockName input) model.portfolioAllocation
            in
            ( { model | portfolioAllocation = portAllocation }, Cmd.none )

        AddStock ->
            let
                id =
                    Dict.size model.portfolioAllocation + 1

                portAllocation =
                    Dict.update id (updateStockName "") model.portfolioAllocation
            in
            ( { model | portfolioAllocation = portAllocation }, Cmd.none )

        RemoveStock id ->
            let
                portAllocation =
                    Dict.update id (\_ -> Nothing) model.portfolioAllocation
            in
            ( { model | portfolioAllocation = portAllocation }, Cmd.none )

        SubmittedForm ->
            let
                symbols =
                    Dict.values model.portfolioAllocation
                        |> List.map (\stock -> stock.name)
            in
            ( model, getStockData symbols )

        GotStockData result ->
            case result |> Debug.log "gotStockData" of
                Ok stockPrice ->
                    ( { model | stockPerformance = Dict.empty }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


apiUrl : List String -> String
apiUrl symbolsInput =
    let
        symbols =
            String.join "," symbolsInput
    in
    Url.custom
        (Url.CrossOrigin "https://api.worldtradingdata.com")
        [ "api", "v1", "history_multi_single_day" ]
        [ Url.string "symbol" symbols, Url.string "date" "2013-03-20", Url.string "api_token" "enAjhSXYaOW5nMV2y0r4Q7GozCk6C4SRTSNlwfNdjUvK9tqu4tCAqLcnopyD" ]
        Nothing


getStockData : List String -> Cmd Msg
getStockData symbolsInput =
    Http.get
        { url = apiUrl symbolsInput
        , expect = Http.expectJson GotStockData decoder
        }


decoder : D.Decoder String
decoder =
    D.at [ "data", "AAPL", "close" ] D.string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        initialBalance =
            String.fromInt model.initialBalance
    in
    Html.form [ onSubmit SubmittedForm ]
        [ fieldset [] [ viewInput "text" "Start Date" model.startDate StartDate ]
        , fieldset [] [ viewInput "number" "Initial Balance" initialBalance InitialBalance ]
        , fieldset []
            [ text "Portfolio Allocation"
            , button [ onClick AddStock ] [ text "Add Stock" ]
            ]
        , div [] (viewStockInput model)
        , button [] [ text "Submit" ]
        ]


viewStockInput : Model -> List (Html Msg)
viewStockInput model =
    Dict.toList model.portfolioAllocation
        |> List.map
            (\( id, { name, allocation } ) ->
                fieldset []
                    [ viewInput "text" "Stock Name" name (PortfolioAllocationName id)
                    , viewInput "number" "Allocation" (String.fromInt allocation) (PortfolioAllocation id)
                    , button [ onClick (RemoveStock id) ] [ text "Remove" ]
                    ]
            )


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    span []
        [ label [] [ text p ]
        , input [ type_ t, placeholder p, value v, onInput toMsg ] []
        ]
