module View exposing (Msg(..), NotificationStatus(..), view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

type Msg
  = None

type NotificationStatus
  = Unsupported
  | Denied
  | Granted
  | Unknown

view model =
  div []
    [ text <| toString model.notificationStatus
    ]
