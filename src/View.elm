module View exposing (Msg(..), Broadcast, Message, view)

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
  = None

type alias Broadcast =
  { title : String
  , description : String
  , actualStartTime : Maybe Time
  , liveChatId : String
  }

type alias Message =
  { authorDisplayName : String
  , publishedAt : Time
  , displayMessage : String
  }

view model =
  div []
    [ header [] [loginView model]
    , div [] [text <| toString model.notificationStatus]
    , div [] [text <| (model.broadcast |> Maybe.map .title |> Maybe.withDefault "--")]
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

messageView : Message -> Html msg
messageView message =
  li []
    [ text message.authorDisplayName
    , text ": "
    , text message.displayMessage
    ]
