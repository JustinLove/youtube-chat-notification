module GoogleApis.Oauth2V1.Decode exposing
  ( TokenInfo
  , tokenInfo
  , sampleTokenInfo
  )

import Json.Decode exposing (..)

type alias TokenInfo =
  { audience : String
  , user_id : Maybe String
  , scope : List String
  , expires_in : Int
  }

tokenInfo : Decoder TokenInfo
tokenInfo =
  map4 TokenInfo
    (field "audience" string)
    (maybe (field "user_id" string))
    (field "scope" scope)
    (field "expires_in" int)

scope : Decoder (List String)
scope =
  string |> map (String.split " ")

sampleTokenInfo = """
{
  "audience":"8819981768.apps.googleusercontent.com",
  "user_id":"123456789",
  "scope":"https://www.googleapis.com/auth/youtube",
  "expires_in":436
}
"""
