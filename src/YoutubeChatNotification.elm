module YoutubeChatNotification exposing (..)

import View exposing (Broadcast, Message)
import Notification exposing (NotificationStatus(..))
import YoutubeId
import GoogleApis.Oauth2V1.Decode as GoogleApis
import Youtube.DataV3.Decode as Youtube

import Html
import Navigation exposing (Location)
import Uuid exposing (Uuid)
import Random.Pcg as Random
import Http
import Json.Decode
import Time exposing (Time)

smallestPollingInterval = 10 * Time.second

type Msg
  = GotNotificationStatus NotificationStatus
  | CurrentUrl Location
  | MessageUpdate Time
  | AuthState Uuid
  | TokenInfo String (Result Http.Error (GoogleApis.TokenInfo))
  | GotLiveBroadcasts (Result Http.Error (Youtube.LiveBroadcastListResponse))
  | GotLiveChatMessages (Result Http.Error (Youtube.LiveChatMessageListResponse))
  | UI (View.Msg)

type alias Model =
  { notificationStatus : NotificationStatus
  , location : Location
  , requestState : Maybe Uuid
  , auth : Maybe String
  , broadcast : Maybe Broadcast
  , messages : List Message
  , messagePageToken : Maybe String
  , messagePollingInterval : Time
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
    , broadcast = Nothing
    , messages = []
    , messagePageToken = Nothing
    , messagePollingInterval = Time.second
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
          Notification.requestPermission ()
        else
          Cmd.none --Notification.send "hi"
      )
    CurrentUrl location ->
      ({model | location = location}, Cmd.none)
    MessageUpdate _ ->
      (model, updateChatMessages model)
    AuthState uuid ->
      ({model | requestState = Just uuid}, Cmd.none)
    TokenInfo token (Ok info) ->
      let _ = Debug.log "token expires in " info.expires_in in
      ({model | auth = Just token}, fetchLiveBroadcasts (Just token))
    TokenInfo _ (Err err) ->
      let _ = Debug.log "access token validation failed" err in
      ({model | auth = Nothing}, Cmd.none)
    GotLiveBroadcasts (Ok response) ->
      let
        mbroadcast = List.head response.items |> Maybe.map myBroadcast
        m2 = {model | broadcast = mbroadcast }
      in
      (m2, updateChatMessages m2)
    GotLiveBroadcasts (Err err) ->
      let _ = Debug.log "fetch broadcasts failed" err in
      (model, Cmd.none)
    GotLiveChatMessages (Ok response) ->
      let
        received = response.items |> List.map myMessage
      in
      ( { model
        | messages = List.append model.messages received
        , messagePageToken = response.nextPageToken
        , messagePollingInterval = max smallestPollingInterval (toFloat response.pollingIntervalMillis)
        }
      , if model.messagePageToken /= Nothing then
        Cmd.batch
          <| List.map (\m -> Notification.send (m.authorDisplayName ++ ": " ++ m.displayMessage)) received
        else
          Cmd.none
      )
    GotLiveChatMessages (Err err) ->
      let _ = Debug.log "fetch chat failed" err in
      (model, Cmd.none)
    UI (View.Update) ->
      (model, updateChatMessages model)

myBroadcast : Youtube.LiveBroadcast -> Broadcast
myBroadcast {snippet} =
  { title = snippet.title
  , description = snippet.description
  , actualStartTime = snippet.actualStartTime
  , liveChatId = snippet.liveChatId
  }

myMessage : Youtube.LiveChatMessage -> Message
myMessage {snippet, authorDetails} =
  case snippet of
    Youtube.TextMessageEvent {publishedAt, messageText} ->
      { authorDisplayName = authorDetails.displayName
      , publishedAt = publishedAt
      , displayMessage = messageText
      }
    Youtube.UnknownEvent messageType publishedAt ->
      { authorDisplayName = "unknown event"
      , publishedAt = publishedAt
      , displayMessage = messageType
      }

updateChatMessages : Model -> Cmd Msg
updateChatMessages model =
  model.broadcast
    |> Maybe.map (\cast -> fetchLiveChatMessages model.auth cast.liveChatId model.messagePageToken)
    |> Maybe.withDefault Cmd.none

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Notification.status GotNotificationStatus
    , Time.every model.messagePollingInterval MessageUpdate
    ]

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
    { auth = auth
    , decoder = Youtube.liveBroadcastListResponse
    , tagger = GotLiveBroadcasts
    , url = liveBroadcastsUrl
    }

liveChatMessagesUrl : String -> Maybe String -> String
liveChatMessagesUrl liveChatId mpageToken=
  let
    page = mpageToken
      |> Maybe.map (\token -> "&pageToken="++token)
      |> Maybe.withDefault ""
  in
  "https://www.googleapis.com/youtube/v3/liveChat/messages?part=snippet,authorDetails&liveChatId=" ++ liveChatId ++ page ++ "&key=" ++ YoutubeId.apikey

fetchLiveChatMessages : Maybe String -> String -> Maybe String -> Cmd Msg
fetchLiveChatMessages auth liveChatId mpageToken =
  youtube
    { auth = auth
    , decoder = Youtube.liveChatMessageListResponse
    , tagger = GotLiveChatMessages
    , url = liveChatMessagesUrl liveChatId mpageToken
    }

youtube :
  { auth : Maybe String
  , decoder : Json.Decode.Decoder a
  , tagger : ((Result Http.Error a) -> msg)
  , url : String
  } -> Cmd msg
youtube {auth, decoder, tagger, url} =
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
