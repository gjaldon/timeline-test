module Main exposing (Model, Msg(..), init, main, update, view, viewInput)

import Browser
import Date exposing (Date)
import DatePicker
import Dict exposing (Dict)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D
import Platform.Cmd
import Time exposing (Weekday(..))
import Url.Builder as Url


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


type alias Model =
    { startDate : Maybe Date
    , initialBalance : Maybe Float
    , portfolioAllocation : Dict Int StockField
    , datePicker : DatePicker.DatePicker
    , currentWorth : Dict String Float
    , stocks : Dict String Stock
    , showResults : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init

        model =
            { startDate = Nothing
            , initialBalance = Nothing
            , portfolioAllocation = Dict.fromList [ ( 1, StockField "" Nothing ) ]
            , datePicker = datePicker
            , currentWorth = Dict.empty
            , stocks = Dict.empty
            , showResults = False
            }
    in
    ( model, Cmd.map ToDatePicker datePickerFx )


settings : DatePicker.Settings
settings =
    let
        isDisabled date =
            List.member (Date.weekday date) [ Sat, Sun ]

        defaultSettings =
            DatePicker.defaultSettings
    in
    { defaultSettings | isDisabled = isDisabled, inputId = Just "start-date", inputClassList = [ ( "form-input", True ) ] }


type alias StockField =
    { name : String
    , allocation : Maybe Int
    }


updateStockAllocation : Maybe Int -> Maybe StockField -> Maybe StockField
updateStockAllocation allocation value =
    case value of
        Nothing ->
            Just (StockField "" allocation)

        Just i ->
            Just { i | allocation = allocation }


updateStockName : String -> Maybe StockField -> Maybe StockField
updateStockName name value =
    case value of
        Nothing ->
            Just (StockField name Nothing)

        Just i ->
            Just { i | name = name }


type Msg
    = InitialBalance String
    | PortfolioAllocation Int String
    | PortfolioAllocationName Int String
    | AddStock
    | RemoveStock Int
    | SubmittedForm
    | GotLatestStockData (Result Http.Error (List DecodedStock))
    | GotHistoricStockData (Result Http.Error (List DecodedStock))
    | ToDatePicker DatePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialBalance input ->
            let
                initialBalance =
                    String.toFloat input
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
                    Dict.update id (String.toUpper input |> updateStockName) model.portfolioAllocation
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
                allocations =
                    Dict.values model.portfolioAllocation

                symbols =
                    List.map (\stock -> stock.name) allocations

                stocks =
                    List.foldl (addStockWithShares model) Dict.empty allocations
            in
            ( { model | stocks = stocks }, Cmd.batch [ getHistoricStockData model.startDate symbols, getLatestStockData symbols ] )

        ToDatePicker subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update settings subMsg model.datePicker

                newDate =
                    case dateEvent of
                        DatePicker.Picked changedDate ->
                            Just changedDate

                        _ ->
                            model.startDate
            in
            ( { model
                | startDate = newDate
                , datePicker = newDatePicker
              }
            , Cmd.none
            )

        GotHistoricStockData result ->
            case result of
                Ok stockData ->
                    let
                        newStocks =
                            List.foldr (\stock dict -> Dict.update stock.symbol (updateStockHistoricPrice stock.price) dict) model.stocks stockData
                    in
                    ( { model | stocks = newStocks, showResults = True }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotLatestStockData result ->
            case result of
                Ok stockData ->
                    let
                        newStocks =
                            List.foldr (\stock dict -> Dict.update stock.symbol (updateStockLatestPrice stock.price) dict) model.stocks stockData
                    in
                    ( { model | stocks = newStocks }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


type alias Stock =
    { symbol : String
    , initialInvestment : Float
    , historicPrice : Float
    , latestPrice : Float
    , currentWorth : Float
    }


addStockWithShares : Model -> StockField -> Dict String Stock -> Dict String Stock
addStockWithShares { initialBalance } { name, allocation } dict =
    let
        floatAllocation =
            (Maybe.withDefault 0 allocation |> toFloat) / 100.0

        initialInvestment =
            floatAllocation * Maybe.withDefault 0 initialBalance
    in
    Dict.insert name (Stock name initialInvestment 0 0 0) dict


updateStockHistoricPrice : Float -> Maybe Stock -> Maybe Stock
updateStockHistoricPrice price value =
    case value of
        Nothing ->
            -- TODO : Raise an error here since this is unexpected behavior
            Just (Stock "" 0 0 0 0)

        Just stock ->
            let
                newStock =
                    { stock | historicPrice = price }

                currentWorth =
                    computeCurrentWorth newStock
            in
            Just { newStock | currentWorth = currentWorth }


updateStockLatestPrice : Float -> Maybe Stock -> Maybe Stock
updateStockLatestPrice price value =
    case value of
        Nothing ->
            -- TODO : Raise an error here since this is unexpected behavior
            Just (Stock "" 0 0 0 0)

        Just stock ->
            Just { stock | latestPrice = price }


computeCurrentWorth : Stock -> Float
computeCurrentWorth stock =
    let
        { initialInvestment, historicPrice, latestPrice } =
            stock
    in
    (initialInvestment / historicPrice) * latestPrice


getStockAllocation model =
    Dict.get model.portfolioAllocation


apiToken : String
apiToken =
    "enAjhSXYaOW5nMV2y0r4Q7GozCk6C4SRTSNlwfNdjUvK9tqu4tCAqLcnopyD"


getLatestStockData : List String -> Cmd Msg
getLatestStockData symbolsInput =
    Http.get
        { url = latestStockUrl symbolsInput
        , expect = Http.expectJson GotLatestStockData latestStocksDecoder
        }


type alias DecodedStock =
    { symbol : String
    , price : Float
    }


latestStocksDecoder : D.Decoder (List DecodedStock)
latestStocksDecoder =
    D.field "data" (D.list latestStockDecoder)


latestStockDecoder : D.Decoder DecodedStock
latestStockDecoder =
    D.map2
        DecodedStock
        (D.field "symbol" D.string)
        (D.field "close_yesterday" D.string
            |> D.map String.toFloat
            |> D.map (Maybe.withDefault 0)
        )


latestStockUrl : List String -> String
latestStockUrl symbolsInput =
    let
        symbols =
            String.join "," symbolsInput
    in
    Url.custom
        (Url.CrossOrigin "https://api.worldtradingdata.com")
        [ "api", "v1", "stock" ]
        [ Url.string "symbol" symbols, Url.string "api_token" apiToken ]
        Nothing


getHistoricStockData : Maybe Date -> List String -> Cmd Msg
getHistoricStockData date symbolsInput =
    Http.get
        { url = historicStockUrl date symbolsInput
        , expect = Http.expectJson GotHistoricStockData historicStockDecoder
        }


historicStockUrl : Maybe Date -> List String -> String
historicStockUrl startDate symbolsInput =
    let
        date =
            Maybe.withDefault (Date.fromOrdinalDate 2019 1) startDate |> Date.toIsoString

        symbols =
            String.join "," symbolsInput
    in
    Url.custom
        (Url.CrossOrigin "https://api.worldtradingdata.com")
        [ "api", "v1", "history_multi_single_day" ]
        [ Url.string "symbol" symbols, Url.string "date" date, Url.string "api_token" apiToken ]
        Nothing


type alias StockInfo =
    { date : String
    , data : Dict String Float
    }


historicStockDecoder : D.Decoder (List DecodedStock)
historicStockDecoder =
    D.map stockInfoToStock stockInfoDecoder


stockInfoToStock : StockInfo -> List DecodedStock
stockInfoToStock { data, date } =
    Dict.foldl
        (\name price stockPerformances ->
            DecodedStock name price :: stockPerformances
        )
        []
        data


stockInfoDecoder : D.Decoder StockInfo
stockInfoDecoder =
    let
        closeField =
            D.field "close" D.string
                |> D.map String.toFloat
                |> D.map (Maybe.withDefault 0)
    in
    D.map2
        StockInfo
        (D.field "date" D.string)
        (D.field "data" (D.dict closeField))


view : Model -> Html Msg
view model =
    let
        showResults =
            if model.showResults then
                viewResults model

            else
                div [] []
    in
    div [ class "column col-12" ]
        [ h1 [] [ text "Portfolio Performance" ]
        , viewForm model
        , showResults
        ]


viewResults : Model -> Html Msg
viewResults model =
    let
        stocks =
            Dict.values model.stocks

        initialBalance =
            Maybe.withDefault 0 model.initialBalance

        remainingBalance =
            initialBalance - List.foldl (\stock sum -> stock.initialInvestment + sum) 0 stocks

        totalCurrentWorth =
            List.foldl (\stock sum -> stock.currentWorth + sum) 0 stocks |> (+) remainingBalance

        totalPerformance =
            ((totalCurrentWorth / initialBalance) * 100) - 100
    in
    section []
        [ h2 [] [ text "Results" ]
        , ul []
            (List.map
                (\stock ->
                    let
                        performance =
                            format usLocale (((stock.currentWorth / stock.initialInvestment) * 100) - 100)

                        currentWorth =
                            format usLocale stock.currentWorth
                    in
                    li []
                        [ text stock.symbol
                        , div [] [ text ("Initial Investment: USD " ++ format usLocale stock.initialInvestment) ]
                        , div [] [ text ("Current Worth: USD " ++ currentWorth) ]
                        , div [] [ text ("Performance: " ++ performance ++ "%") ]
                        ]
                )
                stocks
            )
        , section []
            [ h4 [] [ text "Totals" ]
            , div [] [ text ("Total Current Worth: USD " ++ format usLocale totalCurrentWorth) ]
            , div [] [ text ("Total Performance: " ++ format usLocale totalPerformance ++ "%") ]
            ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    let
        initialBalance =
            Maybe.map String.fromFloat model.initialBalance |> Maybe.withDefault ""
    in
    Html.form [ onSubmit SubmittedForm, class "form-horizontal" ]
        [ viewDatePicker model
        , viewInput "Initial Balance (USD)"
            "initial-balance"
            [ type_ "number"
            , placeholder "100,000"
            , value initialBalance
            , onInput InitialBalance
            , Html.Attributes.min "1"
            ]
        , div [ class "port-allocation" ]
            [ h3 [ class "port-allocation-header" ] [ text "Portfolio Allocation" ]
            , button [ class "add-stock-btn", onClick AddStock, class "btn" ] [ text "Add Stock" ]
            ]
        , div [] (viewStockFields model)
        , div [] [ button [ class "btn btn-primary" ] [ text "Submit" ] ]
        ]


viewDatePicker : Model -> Html Msg
viewDatePicker { startDate, datePicker } =
    div [ class "form-group" ]
        [ div [ class "col-3" ] [ label [ for "start-date", class "form-inline form-label" ] [ text "Start Date" ] ]
        , div [ class "col-9" ] [ DatePicker.view startDate settings datePicker |> Html.map ToDatePicker ]
        ]


viewStockFields : Model -> List (Html Msg)
viewStockFields model =
    Dict.toList model.portfolioAllocation
        |> List.map
            (\( id_, { name, allocation } ) ->
                let
                    newName =
                        String.toUpper name

                    allocationText =
                        Maybe.map String.fromInt allocation |> Maybe.withDefault ""

                    allocationAttrs =
                        [ type_ "number"
                        , placeholder "50"
                        , value allocationText
                        , onInput (PortfolioAllocation id_)
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "100"
                        , id "allocation"
                        , class "form-input"
                        ]

                    symbolAttrs =
                        [ type_ "text", placeholder "AAPL", value newName, onInput (PortfolioAllocationName id_), id "stock-name", class "form-input" ]
                in
                div [ class "form-group stock-field" ]
                    [ div [ class "form-group stock-name-group" ]
                        [ div [ class "col-4" ] [ label [ class "form-label", for "stock-name" ] [ text "Stock Symbol" ] ]
                        , div [ class "col-8" ] [ input symbolAttrs [] ]
                        ]
                    , div [ class "form-group allocation-group" ]
                        [ div [ class "col-6" ] [ label [ class "form-label", for "allocation" ] [ text "Allocation" ] ]
                        , div [ class "col-6" ] [ input allocationAttrs [] ]
                        ]
                    , button [ onClick (RemoveStock id_), class "btn stock-remove-btn" ] [ text "Remove" ]
                    ]
            )


viewInput : String -> String -> List (Attribute msg) -> Html msg
viewInput t id_ attributes =
    let
        newAttributes =
            List.append [ id id_, class "form-input" ] attributes
    in
    div [ class "form-group" ]
        [ div [ class "col-3" ] [ label [ class "form-label", for id_ ] [ text t ] ]
        , div [ class "col-9" ] [ input newAttributes [] ]
        ]
