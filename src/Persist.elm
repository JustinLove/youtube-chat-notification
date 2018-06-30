module Persist exposing (Persist)

import Uuid exposing (Uuid)

type alias Persist =
  { authState : Maybe Uuid
  , popupNotificationActive : Bool
  , audioNoticeActive : Bool
  , audioNoticeIdle : Int
  }
