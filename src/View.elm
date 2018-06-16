module View exposing (Msg(..), view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, onCheck)

type Msg
  = None

view model =
  div []
    [ text "view"
    ]
