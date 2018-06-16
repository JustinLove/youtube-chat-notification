port module Harbor exposing (..)

port notificationStatus : (String -> msg) -> Sub msg
port notificationRequestPermission : () -> Cmd msg
port notificationSend : String -> Cmd msg
