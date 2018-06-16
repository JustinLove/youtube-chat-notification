module YoutubeChatNotification exposing (..)

import View exposing (NotificationStatus(..))
import Harbor

import Html

type Msg
  = GotNotificationStatus NotificationStatus
  | UI (View.Msg)

type alias Model =
  { notificationStatus : NotificationStatus
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
  ( { notificationStatus = Unknown
    }
  , Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotNotificationStatus status ->
      ({model | notificationStatus = status}, Cmd.none)
    UI (View.None) ->
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Harbor.notificationStatus receiveNotificationStatus
    ]

receiveNotificationStatus : String -> Msg
receiveNotificationStatus status =
  GotNotificationStatus
    <| case status of
      "unsupported" -> Unsupported
      "denied" -> Denied
      "granted" -> Granted
      "default" -> Unknown
      _ -> Unknown
