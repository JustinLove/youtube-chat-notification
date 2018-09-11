port module Notification exposing
  ( NotificationStatus(..)
  , status
  , requestPermission
  , send
  )

type NotificationStatus
  = Unsupported
  | Denied
  | Granted
  | Unknown

mapStatus : String -> NotificationStatus
mapStatus name =
  case name of
    "unsupported" -> Unsupported
    "denied" -> Denied
    "granted" -> Granted
    "default" -> Unknown
    _ -> Unknown

status : (NotificationStatus -> msg) -> Sub msg
status tagger = 
  notificationStatus (mapStatus >> tagger)

requestPermission : () -> Cmd msg
requestPermission = notificationRequestPermission

send : String -> Cmd msg
send = notificationSend

port notificationStatus : (String -> msg) -> Sub msg
port notificationRequestPermission : () -> Cmd msg
port notificationSend : String -> Cmd msg
