module GoogleApis.Oauth2V1.Decode exposing
  ( TokenInfo
  , tokenInfo
  , sampleTokenInfo
  , AccessToken
  , accessToken
  , sampleAccessToken
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

type alias AccessToken =
  { access_token : String
  , token_type : String
  , expires_in : Int
  , refresh_token : Maybe String
  }

accessToken : Decoder AccessToken
accessToken =
  map4 AccessToken
    (field "access_token" string)
    (field "token_type" string)
    (field "expires_in" int)
    (maybe (field "refresh_token" string))

sampleAccessToken = """
{
  "access_token" : "ya29.AHES6ZTtm7SuokEB-RGtbBty9IIlNiP9-eNMMQKtXdMP3sfjL1Fc",
  "token_type" : "Bearer",
  "expires_in" : 3600,
  "refresh_token" : "1/HKSmLFXzqP0leUihZp2xUt3-5wkU7Gmu2Os_eBnzw74"
}
"""
