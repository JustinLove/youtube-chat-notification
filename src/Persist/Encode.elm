module Persist.Encode exposing (persist)

import Persist exposing (Persist)
import Uuid exposing (Uuid)

import Json.Encode exposing (..)

persist : Persist -> Value
persist p =
  object
    [ ("authState", maybe Uuid.encode p.authState)
    , ("popupNotificationActive", bool p.popupNotificationActive)
    , ("audioNoticeActive", bool p.audioNoticeActive)
    , ("audioNoticeIdle", int p.audioNoticeIdle)
    ]

maybe : (a -> Value) -> Maybe a -> Value
maybe encode m =
  case m of
    Just v -> encode v
    Nothing -> null
