import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)



main =
  Browser.sandbox { init = init, update = update, view = view }



type alias Model =
  { startDate : String
  , initialBalance : Int
  , portfolioAllocation : Dict String Int
  }



init : Model
init =
  Model "" 0 Dict.empty



type Msg
  = StartDate String
  | InitialBalance String
  | PortfolioAllocation (Dict String Int)



update : Msg -> Model -> Model
update msg model =
  case msg of
    StartDate startDate ->
      { model | startDate = startDate }

    InitialBalance input ->
      let
        initialBalance = Maybe.withDefault 0 (String.toInt input)
      in
      { model | initialBalance = initialBalance }

    PortfolioAllocation portfolioAllocation ->
      { model | portfolioAllocation = portfolioAllocation }



view : Model -> Html Msg
view model =
  let
    initialBalance = String.fromInt model.initialBalance
  in
  div []
    [ viewInput "text" "Start Date" model.startDate StartDate
    , viewInput "number" "Initial Balance" initialBalance InitialBalance
    -- , viewInput "number" "Re-enter Password" model.passwordAgain PasswordAgain
    ]



viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []
