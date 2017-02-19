import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Algo.PrimeAlgo as PrimeAlgo

--main
main =
  program {init = init, update = update, view = view, subscriptions = subscriptions}

--init
type alias Model = {
  start : Int,
  end : Int,
  primeNumbers : List Int
}

init : (Model, Cmd Msg)
init =
  (Model 0 0 [], Cmd.none)

--update
type Msg =
  UpdateStart String |
  UpdateEnd String |
  ListOutPrimeNumbers

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateStart num ->
      ({model | start = (getInt num)}, Cmd.none)
    UpdateEnd num ->
      ({model | end = (getInt num)}, Cmd.none)
    ListOutPrimeNumbers ->
      ({model | primeNumbers = (PrimeAlgo.getPrimeNumbers model.start model.end)}, Cmd.none)

getInt : String -> Int
getInt num =
  case String.toInt num of
    Ok n ->
      n
    Err _ ->
      0

--view
view : Model -> Html Msg
view model =
  div [style [("text-align", "center")]][
    h1 [][text "Adi's prime number generator"],
    label [for "start-input"][text "Start: "],
    input [name "start-input", type_ "text", onInput UpdateStart, value (toString model.start)][],
    br [][],
    label [for "end-input"][text "End: "],
    input [name "end-input", type_ "text", onInput UpdateEnd, value (toString model.end)][],
    br [][],
    input [type_ "button", onClick ListOutPrimeNumbers, value "List out!"][],
    br [][],
    ul [style [("list-style-type", "none")]](List.map getListItem model.primeNumbers)
  ]

getListItem : Int -> Html Msg
getListItem num =
  li [style [("display", "inline")]][text ((toString num)++"   ")]

--subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
