module Youtube.DataV3.Decode exposing
  ( LiveBroadcastListResponse
  , LiveBroadcast
  , liveBroadcastListResponse
  , sampleLiveBroadcastListResponse
  )

import Json.Decode exposing (..)
import Time exposing (Time)
import Date
import Dict exposing (Dict)

type alias LiveBroadcastListResponse =
  { etag : String
  , nextPageToken : Maybe String
  , prevPageToken : Maybe String
  , pageInfo : PageInfo
  , items : List LiveBroadcast
  }

liveBroadcastListResponse : Decoder LiveBroadcastListResponse
liveBroadcastListResponse =
  map5 LiveBroadcastListResponse
    (field "etag" string)
    (maybe (field "nextPageToken" string))
    (maybe (field "prevPageToken" string))
    (field "pageInfo" pageInfo)
    (field "items" (list liveBroadcast))

type alias LiveBroadcast =
  { etag : String
  , id : String
  , snippet : Snippet
  --, status : Maybe Status
  --, contentDetails : Maybe ContentDetails
  --, statistics : Maybe Statistics
  }

liveBroadcast : Decoder LiveBroadcast
liveBroadcast =
  map3 LiveBroadcast
    (field "etag" string)
    (field "id" string)
    (field "snippet" snippet)
    --(field "status" maybe status)
    --(field "contentDetails" maybe contentDetails)
    --(field "statistics" maybe statistics)

type alias Snippet =
  { publishedAt : Time
  , channelId : String
  , title : String
  , description : String
  , thumbnails : Dict String Thumbnail
  , scheduledStartTime : Time
  , scheduledEndTime : Maybe Time
  , actualStartTime : Maybe Time
  , actualEndTime : Maybe Time
  , isDefaultBroadcast : Bool
  , liveChatId : String
  }

snippet : Decoder Snippet
snippet =
  succeed Snippet
    |> map2 (|>) (field "publishedAt" timeStamp)
    |> map2 (|>) (field "channelId" string)
    |> map2 (|>) (field "title" string)
    |> map2 (|>) (field "description" string)
    |> map2 (|>) (field "thumbnails" (dict thumbnail))
    |> map2 (|>) (field "scheduledStartTime" timeStamp)
    |> map2 (|>) (maybe (field "actualStartTime" timeStamp))
    |> map2 (|>) (maybe (field "scheduledEndTime" timeStamp))
    |> map2 (|>) (maybe (field "actualEndTime" timeStamp))
    |> map2 (|>) (field "isDefaultBroadcast" bool)
    |> map2 (|>) (field "liveChatId" string)

type alias Thumbnail =
  { url : String
  , width : Int
  , height : Int
  }

thumbnail : Decoder Thumbnail
thumbnail =
  map3 Thumbnail
    (field "url" string)
    (field "width" int)
    (field "height" int)

sampleLiveBroadcastListResponse = """
{
 "kind": "youtube#liveBroadcastListResponse",
 "etag": "\\"DuHzAJ-eQIiCIp7p4ldoVcVAOeY/NIpsJgnc4FdddZ_MN1G8tEmrdjI\\"",
 "pageInfo": {
  "totalResults": 1,
  "resultsPerPage": 5
 },
 "items": [
  {
   "kind": "youtube#liveBroadcast",
   "etag": "\\"DuHzAJ-eQIiCIp7p4ldoVcVAOeY/qoH-JhUSVkegnrQ64oYGV2XF-tM\\"",
   "id": "WIsThmQQ47I",
   "snippet": {
    "publishedAt": "2018-06-16T00:15:56.000Z",
    "channelId": "UCQIdF_YQKMb-19Ho8o5Wbxg",
    "title": "Elm; Page notifictions and Youtube chat API",
    "description": "Multistreaming with https://restream.io/",
    "thumbnails": {
     "default": {
      "url": "https://i.ytimg.com/vi/WIsThmQQ47I/default_live.jpg",
      "width": 120,
      "height": 90
     },
     "medium": {
      "url": "https://i.ytimg.com/vi/WIsThmQQ47I/mqdefault_live.jpg",
      "width": 320,
      "height": 180
     },
     "high": {
      "url": "https://i.ytimg.com/vi/WIsThmQQ47I/hqdefault_live.jpg",
      "width": 480,
      "height": 360
     },
     "standard": {
      "url": "https://i.ytimg.com/vi/WIsThmQQ47I/sddefault_live.jpg",
      "width": 640,
      "height": 480
     },
     "maxres": {
      "url": "https://i.ytimg.com/vi/WIsThmQQ47I/maxresdefault_live.jpg",
      "width": 1280,
      "height": 720
     }
    },
    "scheduledStartTime": "1970-01-01T00:00:00.000Z",
    "actualStartTime": "2018-06-16T14:00:51.000Z",
    "isDefaultBroadcast": true,
    "liveChatId": "EiEKGFVDUUlkRl9ZUUtNYi0xOUhvOG81V2J4ZxIFL2xpdmU"
   }
  }
 ]
}
"""

type alias PageInfo =
  { totalResults : Int
  , resultsPerPage : Int
  }

pageInfo : Decoder PageInfo
pageInfo =
  map2 PageInfo
    (field "totalResults" int)
    (field "resultsPerPage" int)

timeStamp : Decoder Time
timeStamp =
  string
    |> andThen (\s -> case Date.fromString s of
      Ok d -> succeed (Date.toTime d)
      Err err -> fail err
    )
