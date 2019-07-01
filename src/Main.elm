module Main exposing (Model, Msg(..), init, main, update, view, viewInput)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { startDate : String
    , initialBalance : Int
    , portfolioAllocation : Dict Int Stock
    }


init : Model
init =
    Model "" 0 (Dict.fromList [ ( 1, Stock "" 0 ) ])


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

        AddStock ->
            let
                id =
                    Dict.size model.portfolioAllocation + 1

                portAllocation =
                    Dict.update id (updateStockName "") model.portfolioAllocation
            in
            { model | portfolioAllocation = portAllocation }

        RemoveStock id ->
            let
                portAllocation =
                    Dict.update id (\_ -> Nothing) model.portfolioAllocation
            in
            { model | portfolioAllocation = portAllocation }


view : Model -> Html Msg
view model =
    let
        initialBalance =
            String.fromInt model.initialBalance
    in
    div []
        [ viewInput "text" "Start Date" model.startDate StartDate
        , viewInput "number" "Initial Balance" initialBalance InitialBalance
        , div [] [ button [ onClick AddStock ] [ text "Add Stock" ] ]
        , div [] (viewStockInput model)
        ]


viewStockInput : Model -> List (Html Msg)
viewStockInput model =
    Dict.toList model.portfolioAllocation
        |> List.map
            (\( id, { name, allocation } ) ->
                div []
                    [ viewInput "text" "Stock Name" name (PortfolioAllocationName id)
                    , viewInput "number" "Portfolio Allocation" (String.fromInt allocation) (PortfolioAllocation id)
                    , button [ onClick (RemoveStock id) ] [ text "Remove" ]
                    ]
            )


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []
