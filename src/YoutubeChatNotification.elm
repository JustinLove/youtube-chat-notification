module YoutubeChatNotification exposing (..)

import View exposing (Message, urlForRedirect)
import Notification exposing (NotificationStatus(..))
import YoutubeId
import GoogleApis.Oauth2V1.Decode as GoogleApis
import Youtube.DataV3.Decode as Youtube
import LocalStorage
import Persist exposing (Persist)
import Persist.Encode
import Persist.Decode

import Html
import Navigation exposing (Location)
import Uuid exposing (Uuid)
import Random.Pcg as Random
import Http
import Json.Decode
import Time exposing (Time)
import Task
import Dom.Scroll

smallestPollingInterval = 10 * Time.second
audioNoticeLength = 3 * Time.second
defaultAudioNoticeIdle = 2 * 60

type Msg
  = Noop
  | Loaded (Maybe Persist)
  | GotNotificationStatus NotificationStatus
  | CurrentUrl Location
  | AudioStart Time
  | AudioEnd Time
  | MessageUpdate Time
  | TokenLifetimeStart Time Time
  | TokenExpired Time
  | AuthState Uuid
  | AccessToken (Result Http.Error (GoogleApis.AccessToken))
  | RefreshToken (Result Http.Error (GoogleApis.AccessToken))
  | Revoked (Result Http.Error String)
  | TokenInfo String (Result Http.Error (GoogleApis.TokenInfo))
  | GotLiveBroadcasts (Result Http.Error (Youtube.LiveBroadcastListResponse))
  | GotLiveChatMessages (Result Http.Error (Youtube.LiveChatMessageListResponse))
  | UI (View.Msg)

type alias Model =
  { notificationStatus : NotificationStatus
  , location : Location
  , time : Time
  , responseState : Maybe Uuid
  , requestState : Maybe Uuid
  , auth : Maybe String
  , authExpires : Maybe Time
  , refresh : Maybe String
  , title : Maybe String
  , liveChatId : Maybe String
  , messages : List Message
  , messagePageToken : Maybe String
  , messagePollingInterval : Maybe Time
  , audioNoticeActive : Bool
  , audioNoticeIdle : Int
  , audioNotice : Maybe Time
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
    code = extractSearchArgument "code" location
    state = case code of
      Just _ -> extractSearchArgument "state" location
        |> Maybe.andThen Uuid.fromString
      Nothing -> extractHashArgument "state" location
        |> Maybe.andThen Uuid.fromString
    liveChatId = extractSearchArgument "liveChatId" location
    title = extractSearchArgument "title" location
      |> Maybe.andThen Http.decodeUri
    model =
      { notificationStatus = Unknown
      , location = location
      , time = 0
      , responseState = state
      , requestState = Nothing
      , auth = auth
      , authExpires = Nothing
      , refresh = Nothing
      , title = title
      , liveChatId = liveChatId
      , messages = []
      , messagePageToken = Nothing
      , messagePollingInterval = Nothing
      , audioNoticeActive = True
      , audioNoticeIdle = defaultAudioNoticeIdle
      , audioNotice = Nothing
      }
  in
  ( model
  , Cmd.batch
    [ Random.generate AuthState Uuid.uuidGenerator
    , case code of
      Just string -> exchangeToken location string
      Nothing -> Cmd.none
    , case liveChatId of
      Just id -> updateChatMessages model
      Nothing -> Cmd.none
    ]
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop -> (model, Cmd.none)
    Loaded mstate ->
      ( case mstate of
        Just state ->
          resolveLoaded state model
        Nothing ->
          (model, Cmd.none)
      )
    GotNotificationStatus status ->
      ({model | notificationStatus = status}
      , if status == Unknown then
          Notification.requestPermission ()
        else
          Cmd.none --Notification.send "hi"
      )
    CurrentUrl location ->
      ({model | location = location}, Cmd.none)
    AudioStart time ->
      ({model | audioNotice = Just time}, Cmd.none)
    AudioEnd _ ->
      ({model | audioNotice = Nothing}, Cmd.none)
    MessageUpdate time ->
      ({model | time = time}, updateChatMessages model)
    TokenLifetimeStart expiresIn time ->
      ( {model | authExpires = Just (time+expiresIn), time = time}, Cmd.none)
    TokenExpired _ ->
      ( {model | authExpires = Nothing}
      , case model.refresh of
        Just token -> refreshToken token
        Nothing -> Debug.log "no refresh token" Cmd.none
      )
    AuthState uuid ->
      {model | requestState = Just uuid}
        |> persist
    AccessToken (Ok info) ->
      let
        _ = Debug.log "token expires in " info.expires_in
        _ = Debug.log "refresh token" (info.refresh_token /= Nothing)
      in
      ( {model | auth = Just info.access_token, refresh = info.refresh_token}
      , Cmd.batch
        [ fetchLiveBroadcasts (Just info.access_token)
        , Time.now |> Task.perform (TokenLifetimeStart ((toFloat info.expires_in) * Time.second))
        ]
      )
    AccessToken (Err err) ->
      let _ = Debug.log "access token exchange failed" err in
      ({model | auth = Nothing, refresh = Nothing}, Cmd.none)
    RefreshToken (Ok info) ->
      let _ = Debug.log "token expires in " info.expires_in in
      ( {model | auth = Just info.access_token}
      , Time.now |> Task.perform (TokenLifetimeStart ((toFloat info.expires_in) * Time.second))
      )
    RefreshToken (Err err) ->
      let _ = Debug.log "refresh token failed" err in
      ({model | auth = Nothing, refresh = Nothing}, Cmd.none)
    Revoked (Ok _) ->
      let _ = Debug.log "token revoked" "" in
      ( {model | auth = Nothing, refresh = Nothing}
      , Cmd.none
      )
    Revoked (Err err) ->
      let _ = Debug.log "revoke token failed" err in
      (model, Cmd.none)
    TokenInfo token (Ok info) ->
      let _ = Debug.log "token expires in " info.expires_in in
      ( {model | auth = Just token}
      , Cmd.batch
        [ fetchLiveBroadcasts (Just token)
        , Time.now |> Task.perform (TokenLifetimeStart ((toFloat info.expires_in) * Time.second))
        ]
      )
    TokenInfo _ (Err err) ->
      let _ = Debug.log "access token validation failed" err in
      ({model | auth = Nothing}, Cmd.none)
    GotLiveBroadcasts (Ok response) ->
      let
        mbroadcast = List.head response.items |> Maybe.map .snippet
        m2 =
          { model
          | liveChatId = mbroadcast |> Maybe.map .liveChatId
          , title = mbroadcast |> Maybe.map .title
          }
      in
      ( m2
      , Cmd.batch
        [ Navigation.newUrl <| createPath m2
        , updateChatMessages m2
        ]
      )
    GotLiveBroadcasts (Err err) ->
      let _ = Debug.log "fetch broadcasts failed" err in
      (model, Cmd.none)
    GotLiveChatMessages (Ok response) ->
      let
        received = response.items |> List.map myMessage
        initialBatch = model.messagePageToken == Nothing
        messagesReceived = List.length received > 0
        lastTime = List.head (List.reverse model.messages)
          |> Maybe.map (.publishedAt)
          |> Maybe.withDefault 0 
        newTime = List.head received
          |> Maybe.map (.publishedAt)
          |> Maybe.withDefault lastTime
        idleNotice = if newTime - lastTime > ((toFloat model.audioNoticeIdle) * Time.second) then
          (Time.now |> Task.perform AudioStart)
        else
          Cmd.none
      in
      ( { model
        | messages = List.append model.messages received
        , messagePageToken = response.nextPageToken
        , messagePollingInterval = Just <| max smallestPollingInterval (toFloat response.pollingIntervalMillis)
        }
      , Cmd.batch
        [ if messagesReceived && not initialBatch then
            List.map (\m -> Notification.send (m.authorDisplayName ++ ": " ++ m.displayMessage)) received
              |> (::) idleNotice
              |> Cmd.batch
          else
            Cmd.none
        , if messagesReceived then
            Task.attempt (always Noop) <| Dom.Scroll.toBottom "chat-area"
          else
            Cmd.none
        ]
      )
    GotLiveChatMessages (Err err) ->
      let _ = Debug.log "fetch chat failed" err in
      ({model | messagePollingInterval = Nothing}, Cmd.none)
    UI (View.LogOut) ->
      ( {model | auth = Nothing, refresh = Nothing}
      , case model.auth of
          Just token -> revokeToken token
          Nothing -> Cmd.none
      )
    UI (View.SetAudioNoticeIdle time) ->
      ( {model | audioNoticeIdle = time}, Cmd.none)
    UI (View.ToggleAudioNotice) ->
      ( {model | audioNoticeActive = not model.audioNoticeActive}, Cmd.none)

resolveLoaded : Persist -> Model -> (Model, Cmd Msg)
resolveLoaded state model =
  if YoutubeId.checkOauthState == False
     || model.responseState == state.authState then
    ( model
    , case model.auth of
      Just token -> validateToken token
      Nothing -> Cmd.none
    )
  else
    let _ = Debug.log "auth state mismatch" [model.responseState, state.authState] in
    ( { model | auth = Nothing }
    , Cmd.none
    )

persist : Model -> (Model, Cmd Msg)
persist model =
  (model, saveState model)

saveState : Model -> Cmd Msg
saveState model =
  Persist model.requestState
    |> Persist.Encode.persist
    |> LocalStorage.saveJson

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
  model.liveChatId
    |> Maybe.map (\id -> fetchLiveChatMessages model.auth id model.messagePageToken)
    |> Maybe.withDefault Cmd.none

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ LocalStorage.loadedJson Persist.Decode.persist Loaded
    , Notification.status GotNotificationStatus
    , model.messagePollingInterval
      |> Maybe.map (\t -> Time.every t MessageUpdate)
      |> Maybe.withDefault Sub.none
    , (if model.audioNoticeActive then model.audioNotice else Nothing)
      |> Maybe.map (\_ -> Time.every audioNoticeLength AudioEnd)
      |> Maybe.withDefault Sub.none
    , model.authExpires
      |> Maybe.map (\t -> Time.every (t - model.time) TokenExpired)
      |> Maybe.withDefault Sub.none
    ]

exchangeTokenBody : Location -> String -> Http.Body
exchangeTokenBody location code =
  Http.stringBody "application/x-www-form-urlencoded" <|
    ("code=" ++ code ++
    "&redirect_uri=" ++ (Http.encodeUri (urlForRedirect location)) ++
    "&grant_type=authorization_code")

exchangeToken : Location -> String -> Cmd Msg
exchangeToken location code =
  Http.send AccessToken <| Http.request
    { method = "POST"
    , headers = []
    , url = YoutubeId.oauthProxyUrl
    , body = exchangeTokenBody location code
    , expect = Http.expectJson GoogleApis.accessToken
    , timeout = Nothing
    , withCredentials = False
    }

refreshTokenBody : String -> Http.Body
refreshTokenBody token =
  Http.stringBody "application/x-www-form-urlencoded" <|
    ("refresh_token=" ++ token ++ "&grant_type=refresh_token")

refreshToken : String -> Cmd Msg
refreshToken token =
  Http.send RefreshToken <| Http.request
    { method = "POST"
    , headers = []
    , url = YoutubeId.oauthProxyUrl
    , body = refreshTokenBody token
    , expect = Http.expectJson GoogleApis.accessToken
    , timeout = Nothing
    , withCredentials = False
    }

revokeTokenUrl : String -> String
revokeTokenUrl token =
  "https://accounts.google.com/o/oauth2/revoke?token=" ++ token

revokeToken : String -> Cmd Msg
revokeToken token =
  Http.send Revoked <| Http.request
    { method = "GET"
    , headers = []
    , url = revokeTokenUrl token
    , body = Http.emptyBody
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    }

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

extractSearchArgument : String -> Location -> Maybe String
extractSearchArgument key location =
  location.search
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

createQueryString : Model -> String
createQueryString model =
  [ case model.title of
      Just title -> "title=" ++ title
      Nothing -> ""
  , case model.liveChatId of
      Just id -> "liveChatId=" ++ id
      Nothing -> ""
  ]
    |> List.filter ((/=) "")
    |> String.join "&"

createPath : Model -> String
createPath model =
  model.location.pathname ++ "?" ++ (createQueryString model)
