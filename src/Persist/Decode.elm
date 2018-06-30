module Persist.Decode exposing (persist)

import Persist exposing (Persist)
import Uuid exposing (Uuid)

import Json.Decode exposing (..)

persist : Decoder Persist
persist =
  map4 Persist
    (field "authState" (nullable Uuid.decoder))
    (field "popupNotificationActive" bool)
    (field "audioNoticeActive" bool)
    (field "audioNoticeIdle" int)
