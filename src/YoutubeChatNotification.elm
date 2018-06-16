module YoutubeChatNotification exposing (..)

import View exposing (NotificationStatus(..))
import Harbor

import Html
import Navigation exposing (Location)
import Uuid exposing (Uuid)
import Random.Pcg as Random

type Msg
  = GotNotificationStatus NotificationStatus
  | CurrentUrl Location
  | AuthState Uuid
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
    , auth = auth
    }
  , Random.generate AuthState Uuid.uuidGenerator
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
