module View exposing (Msg(..), Message, view, urlForRedirect)

import YoutubeId
import Notification exposing (NotificationStatus(..))

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Navigation exposing (Location)
import Uuid exposing (Uuid)
import Http
import Time exposing (Time)

type Msg
  = LogOut

type alias Message =
  { authorDisplayName : String
  , publishedAt : Time
  , displayMessage : String
  }

view model =
  div []
    [ header [] [loginView model]
    , div [] [text <| toString model.notificationStatus]
    , div [] [text <| (model.title |> Maybe.withDefault "--")]
    , if model.audioNotice /= Nothing then
        audio
          [ autoplay True
          , src "406243__stubb__typewriter-ding-near-mono.wav"
          ] []
      else
        text ""
    , model.messages
      |> List.map messageView
      |> ul []
    ]

loginView model =
  case model.auth of
    Just _ ->
      span []
        --[ span [ class "user", title model.self.id ] [ text model.self.displayName ]
        [ text " "
        , button [ onClick LogOut ] [ text "Log out" ]
        , text " expires: "
        , text <| toString <| round
          (model.authExpires
            |> Maybe.map (\t -> (t - model.time) / Time.second)
            |> Maybe.withDefault 0
          )
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
    |> chop "#"
    |> chop "?"

chop : String -> String -> String
chop char s =
  if String.right 1 s == char then String.dropRight 1 s else s

messageView : Message -> Html msg
messageView message =
  li []
    [ text message.authorDisplayName
    , text ": "
    , text message.displayMessage
    ]
