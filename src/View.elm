module View exposing (Msg(..), NotificationStatus(..), view)

import YoutubeId

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Navigation exposing (Location)
import Uuid exposing (Uuid)
import Http

type Msg
  = None

type NotificationStatus
  = Unsupported
  | Denied
  | Granted
  | Unknown

view model =
  div []
    [ header [] [displayLogin model]
    , div [] [text <| toString model.notificationStatus]
    ]

displayLogin model =
  case model.auth of
    Just _ ->
      span []
        --[ span [ class "user", title model.self.id ] [ text model.self.displayName ]
        [ text " "
        , a [ href (model.location.origin ++ model.location.pathname) ] [ text "logout" ]
        ]
    Nothing ->
      a [ href (authorizeUrl (urlForRedirect model.location) model.requestState) ] [ text "login" ]

authorizeUrl : String -> Maybe Uuid -> String
authorizeUrl redirectUri authState =
  "https://accounts.google.com/o/oauth2/auth"
    ++ "?client_id=" ++ YoutubeId.clientId
    ++ "&redirect_uri=" ++ (Http.encodeUri redirectUri)
    ++ "&response_type=token"
    ++ "&scope=" ++ (Http.encodeUri "https://www.googleapis.com/auth/youtube.readonly")
    ++ (case authState of
      Just uuid -> "&state=" ++ (Uuid.toString uuid)
      Nothing -> "")

urlForRedirect : Location -> String
urlForRedirect location =
  location.href
    |> String.dropRight (String.length location.hash)
    |> String.dropRight (String.length location.search)
