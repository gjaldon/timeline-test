module Main exposing (Model, Msg(..), init, main, update, view, viewInput)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { startDate : String
    , initialBalance : Int
    , portfolioAllocation : Dict Int Stock
    }


init : Model
init =
    Model "" 0 Dict.empty


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


update : Msg -> Model -> Model
update msg model =
    case msg of
        StartDate startDate ->
            { model | startDate = startDate }

        InitialBalance input ->
            let
                initialBalance =
                    Maybe.withDefault 0 (String.toInt input)
            in
            { model | initialBalance = initialBalance }

        PortfolioAllocation id input ->
            let
                allocation =
                    String.toInt input

                portAllocation =
                    Dict.update id (updateStockAllocation allocation) model.portfolioAllocation
            in
            { model | portfolioAllocation = portAllocation }

        PortfolioAllocationName id input ->
            let
                portAllocation =
                    Dict.update id (updateStockName input) model.portfolioAllocation
            in
            { model | portfolioAllocation = portAllocation }


getAllocation : Int -> Model -> String
getAllocation id model =
    case Dict.get id model.portfolioAllocation of
        Nothing ->
            "0"

        Just { allocation } ->
            String.fromInt allocation


getStockName : Int -> Model -> String
getStockName id model =
    case Dict.get id model.portfolioAllocation of
        Nothing ->
            ""

        Just { name } ->
            name


view : Model -> Html Msg
view model =
    let
        initialBalance =
            String.fromInt model.initialBalance
    in
    div []
        [ viewInput "text" "Start Date" model.startDate StartDate
        , viewInput "number" "Initial Balance" initialBalance InitialBalance
        , viewInput "number" "Stock Name" (getStockName 1 model) (PortfolioAllocation 1)
        , viewInput "number" "Portfolio Allocation" (getAllocation 1 model) (PortfolioAllocation 1)
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []
