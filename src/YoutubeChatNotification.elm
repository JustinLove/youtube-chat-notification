module YoutubeChatNotification exposing (..)

import View exposing (NotificationStatus(..))
import Harbor
import YoutubeId
import GoogleApis.Oauth2V1.Decode as GoogleApis

import Html
import Navigation exposing (Location)
import Uuid exposing (Uuid)
import Random.Pcg as Random
import Http
import Json.Decode

type Msg
  = GotNotificationStatus NotificationStatus
  | CurrentUrl Location
  | AuthState Uuid
  | TokenInfo String (Result Http.Error (GoogleApis.TokenInfo))
  | LiveBroadcasts (Result Http.Error (Json.Decode.Value))
  | UI (View.Msg)

type alias Model =
  { notificationStatus : NotificationStatus
  , location : Location
  , requestState : Maybe Uuid
  , auth : Maybe String
  }

main =
  Navigation.program CurrentUrl
    { init = init
    , view = (\model -> Html.map UI (View.view model))
    , update = update
    , subscriptions = subscriptions
    }

init : Location -> (Model, Cmd Msg)
init location =
  let
    auth = extractHashArgument "access_token" location
    state = extractHashArgument "state" location
      |> Maybe.andThen Uuid.fromString
  in
  ( { notificationStatus = Unknown
    , location = location
    , requestState = state
    , auth = Nothing
    }
  , Cmd.batch
    [ Random.generate AuthState Uuid.uuidGenerator
    , case auth of
      Just token -> validateToken token
      Nothing -> Cmd.none
    ]
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotNotificationStatus status ->
      ({model | notificationStatus = status}
      , if status == Unknown then
          Harbor.notificationRequestPermission ()
        else
          Cmd.none --Harbor.notificationSend "hi"
      )
    CurrentUrl location ->
      ({model | location = location}, Cmd.none)
    AuthState uuid ->
      ({model | requestState = Just uuid}, Cmd.none)
    TokenInfo token (Ok info) ->
      let _ = Debug.log "token expires in " info.expires_in in
      ({model | auth = Just token}, fetchLiveBroadcasts (Just token))
    TokenInfo _ (Err err) ->
      let _ = Debug.log "access token validation failed" err in
      ({model | auth = Nothing}, Cmd.none)
    LiveBroadcasts (Ok value) ->
      let _ = Debug.log "live broadcasts" value in
      (model, Cmd.none)
    LiveBroadcasts (Err err) ->
      let _ = Debug.log "fetch broadcasts failed" err in
      (model, Cmd.none)
    UI (View.None) ->
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Harbor.notificationStatus receiveNotificationStatus
    ]

receiveNotificationStatus : String -> Msg
receiveNotificationStatus status =
  GotNotificationStatus
    <| case status of
      "unsupported" -> Unsupported
      "denied" -> Denied
      "granted" -> Granted
      "default" -> Unknown
      _ -> Unknown

validateTokenUrl : String -> String
validateTokenUrl token =
  "https://www.googleapis.com/oauth2/v1/tokeninfo?access_token=" ++ token

validateToken : String -> Cmd Msg
validateToken token =
  Http.send (TokenInfo token) <| Http.request
    { method = "GET"
    , headers = []
    , url = validateTokenUrl token
    , body = Http.emptyBody
    , expect = Http.expectJson GoogleApis.tokenInfo
    , timeout = Nothing
    , withCredentials = False
    }

liveBroadcastsUrl : String
liveBroadcastsUrl =
  "https://www.googleapis.com/youtube/v3/liveBroadcasts?part=snippet&broadcastStatus=active&broadcastType=all&key=" ++ YoutubeId.apikey

fetchLiveBroadcasts : Maybe String -> Cmd Msg
fetchLiveBroadcasts auth =
  youtube
    { clientId = YoutubeId.clientId
    , auth = auth
    , decoder = Json.Decode.value
    , tagger = LiveBroadcasts
    , url = liveBroadcastsUrl
    }

youtube :
  { clientId : String
  , auth : Maybe String
  , decoder : Json.Decode.Decoder a
  , tagger : ((Result Http.Error a) -> msg)
  , url : String
  } -> Cmd msg
youtube {clientId, auth, decoder, tagger, url} =
  Http.send tagger <| Http.request
    { method = "GET"
    , headers = authHeaders auth
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }

authHeaders : Maybe String -> List Http.Header
authHeaders auth =
  case auth of
    Just token ->
      [ Http.header "Authorization" ("Bearer "++token) ]
    Nothing ->
      []

extractHashArgument : String -> Location -> Maybe String
extractHashArgument key location =
  location.hash
    |> String.dropLeft 1
    |> String.split "&"
    |> List.map (String.split "=")
    |> List.filter (\x -> case List.head x of
      Just s ->
        s == key
      Nothing ->
        False)
    |> List.head
    |> Maybe.andThen List.tail
    |> Maybe.andThen List.head
