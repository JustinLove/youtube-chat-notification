module View exposing (Msg(..), Message, view, urlForRedirect)

import YoutubeId
import Notification exposing (NotificationStatus(..))

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onCheck, on)
import Navigation exposing (Location)
import Uuid exposing (Uuid)
import Http
import Time exposing (Time)
import Json.Decode

type Msg
  = LogOut
  | SetAudioNoticeIdle Int
  | ToggleAudioNotice

type alias Message =
  { authorDisplayName : String
  , publishedAt : Time
  , displayMessage : String
  }

css = """
body {background-color: #111; color: #ccc; margin: 0;}
a:link, a:visited { color: #56f; }
a:hover, a:active { color: #ccc; }
header {
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  background-color: #444;
  margin: 0;
  padding: 0.5em;
}
.stream-title {font-weight: bold;}
.config-audio-notice-idle input {width: 4em;}
#chat-area {
  list-style-type: none;
  overflow: auto;
  width: 90%;
  margin: 0.5em;
  padding: 0;
  position: absolute;
  top: 3em;
  bottom: 0;
}
.chat-author {color: #56f; font-weight: bold;}
"""

view model =
  div []
    [ node "style" [] [ text css ]
    , header []
      [ loginView model
      , div [ class "stream-title" ] [text <| (model.title |> Maybe.withDefault "--")]
      , div [ class "config-audio-notice" ]
        [ span [ class "config-audio-notice-active" ]
          [ input
            [ type_ "checkbox"
            , Html.Attributes.name "audio-notice-active"
            , id "audio-notice-active"
            , value "selected"
            , onCheck (\_ -> ToggleAudioNotice)
            , checked model.audioNoticeActive
            ] []
          ]
        , span [ class "config-audio-notice-idle" ]
          [ label [ for "audio-notice-idle" ] [ text "Audio alarm after idle for " ]
          , text " "
          , input
            [ value <| toString model.audioNoticeIdle
            , type_ "number"
            , id "audio-notice-idle"
            , name "audio-notice-idle"
            , Html.Attributes.min "0"
            , on "change" <| targetValue int SetAudioNoticeIdle
            ] []
          , label [ for "audio-notice-idle" ] [ text "s" ]
          ]
        ]
      , div [ class "notification-status" ] [text <| toString model.notificationStatus]
      ]
    , if model.audioNotice /= Nothing then
        audio
          [ autoplay True
          , src "406243__stubb__typewriter-ding-near-mono.wav"
          ] []
      else
        text ""
    , model.messages
      |> List.map messageView
      |> ul [ id "chat-area"]
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
  li [ class "chat-event" ]
    [ span [ class "chat-author" ] [ text message.authorDisplayName]
    , text " "
    , span [ class "chat-message" ] [ text message.displayMessage ]
    ]

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)

int : Json.Decode.Decoder Int
int =
  Json.Decode.string
    |> Json.Decode.andThen (\text ->
      case String.toInt text of
        Ok n -> Json.Decode.succeed n
        Err _ -> Json.Decode.fail "not an integer"
      )
