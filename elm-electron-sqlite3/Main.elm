port module Main exposing (..)

import Browser exposing (element)
import Html exposing (Html, button, div, form, input, text, textarea)
import Html.Attributes exposing (cols, rows, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode

main : Program () Model Msg
main =
  element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Model

type alias Model =
  { input : Maybe String
  , sql : Maybe String
  , records : Maybe String
  }

init : () -> (Model, Cmd Msg)
init flags =
  (Model Nothing Nothing Nothing, Cmd.none)

-- Update

type Msg
  = Input String
  | ClearQuery
  | ExecQueryReq
  | ExecQueryRes String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Input text ->
      ({model | input = Just text}, Cmd.none)
    ClearQuery ->
      init ()
    ExecQueryReq ->
      case model.input of
        Just s  -> (model, execQuery s)
        Nothing -> (model, Cmd.none)
    ExecQueryRes res ->
      ({model | sql = model.input, records = Just res}, Cmd.none)

port execQuery : String -> Cmd msg

port resultSet : (String -> msg) -> Sub msg

-- Subscription

subscriptions : Model -> Sub Msg
subscriptions model =
  resultSet ExecQueryRes

-- View

view : Model -> Html Msg
view model =
  div []
    [ div []
      [ textarea
        [ cols 60, rows 10, placeholder "Query"
        , value <| Maybe.withDefault "" model.input
        , onInput Input
        ]
        []
      ]
    , button [onClick ClearQuery]   [text "Clear"]
    , button [onClick ExecQueryReq] [text "Query"]
    , div [] [text <| Maybe.withDefault "" model.sql]
    , div [] [text <| Maybe.withDefault "" model.records]
    ]
