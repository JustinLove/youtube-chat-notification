module Persist.Decode exposing (persist)

import Persist exposing (Persist)
import Uuid exposing (Uuid)

import Json.Decode exposing (..)

persist : Decoder Persist
persist =
  map Persist
    (field "authState" (nullable Uuid.decoder))
