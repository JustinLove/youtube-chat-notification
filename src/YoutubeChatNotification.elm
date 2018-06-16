module YoutubeChatNotification exposing (..)

import View

import Html

type Msg
  = UI (View.Msg)

type alias Model =
  {
  }

main =
  Html.program
    { init = init
    , view = (\model -> Html.map UI (View.view model))
    , update = update
    , subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init =
  ({}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UI (View.None) ->
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
