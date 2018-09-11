module Youtube.DataV3.Decode exposing
  ( LiveBroadcastListResponse
  , LiveBroadcast
  , liveBroadcastListResponse
  , sampleLiveBroadcastListResponse
  , LiveChatMessageListResponse
  , LiveChatMessage
  , LiveChatMessageSnippet(..)
  , liveChatMessageListResponse
  , sampleLiveChatMessageListResponse
  )

import Json.Decode exposing (..)
import Time exposing (Posix)
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
  , snippet : LiveBroadcastSnippet
  --, status : Maybe Status
  --, contentDetails : Maybe ContentDetails
  --, statistics : Maybe Statistics
  }

liveBroadcast : Decoder LiveBroadcast
liveBroadcast =
  map3 LiveBroadcast
    (field "etag" string)
    (field "id" string)
    (field "snippet" liveBroadcastSnippet)
    --(field "status" maybe status)
    --(field "contentDetails" maybe contentDetails)
    --(field "statistics" maybe statistics)

type alias LiveBroadcastSnippet =
  { publishedAt : Posix
  , channelId : String
  , title : String
  , description : String
  , thumbnails : Dict String Thumbnail
  , scheduledStartTime : Posix
  , scheduledEndTime : Maybe Posix
  , actualStartTime : Maybe Posix
  , actualEndTime : Maybe Posix
  , isDefaultBroadcast : Bool
  , liveChatId : String
  }

liveBroadcastSnippet : Decoder LiveBroadcastSnippet
liveBroadcastSnippet =
  succeed LiveBroadcastSnippet
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

type alias LiveChatMessageListResponse =
  { etag : String
  , nextPageToken : Maybe String
  , pollingIntervalMillis : Int
  , offlineAt : Maybe Posix
  , pageInfo : PageInfo
  , items : List LiveChatMessage
  }

liveChatMessageListResponse : Decoder LiveChatMessageListResponse
liveChatMessageListResponse =
  map6 LiveChatMessageListResponse
    (field "etag" string)
    (maybe (field "nextPageToken" string))
    (field "pollingIntervalMillis" int)
    (maybe (field "offlineAt" timeStamp))
    (field "pageInfo" pageInfo)
    (field "items" (list liveChatMessage))

type alias LiveChatMessage =
  { etag : String
  , id : String
  , snippet : LiveChatMessageSnippet
  , authorDetails : AuthorDetails
  }

liveChatMessage : Decoder LiveChatMessage
liveChatMessage =
  map4 LiveChatMessage
    (field "etag" string)
    (field "id" string)
    (field "snippet" liveChatMessageSnippet)
    (field "authorDetails" authorDetails)

type LiveChatMessageSnippet
  = TextMessageEvent TextMessageEventDetails
  | UnknownEvent String Posix

liveChatMessageSnippet : Decoder LiveChatMessageSnippet
liveChatMessageSnippet =
  (field "type" string)
    |> andThen (\messageType ->
      case messageType of
        "textMessageEvent" ->
          map TextMessageEvent textMessageEventDetails
        _ ->
          map2 UnknownEvent
            (succeed messageType)
            (field "publishedAt" timeStamp)
      )

type alias TextMessageEventDetails =
  { publishedAt : Posix
  , authorChannelId : String
  , messageText : String
  }

textMessageEventDetails : Decoder TextMessageEventDetails
textMessageEventDetails =
  map3 TextMessageEventDetails
    (field "publishedAt" timeStamp)
    (field "authorChannelId" string)
    (at ["textMessageDetails", "messageText"] string)

type alias AuthorDetails =
  { channelId : String
  , channelUrl : String
  , displayName : String
  , profileImageUrl : String
  , isVerified : Bool
  , isChatOwner : Bool
  , isChatSponsor : Bool
  , isChatModerator : Bool
  }

authorDetails : Decoder AuthorDetails
authorDetails =
  succeed AuthorDetails
    |> map2 (|>) (field "channelId" string)
    |> map2 (|>) (field "channelUrl" string)
    |> map2 (|>) (field "displayName" string)
    |> map2 (|>) (field "profileImageUrl" string)
    |> map2 (|>) (field "isVerified" bool)
    |> map2 (|>) (field "isChatOwner" bool)
    |> map2 (|>) (field "isChatSponsor" bool)
    |> map2 (|>) (field "isChatModerator" bool)

sampleLiveChatMessageListResponse = """
{
 "kind": "youtube#liveChatMessageListResponse",
 "etag": "\\"DuHzAJ-eQIiCIp7p4ldoVcVAOeY/kMSBWSl6lnocPzvkZ_wvg_UuDLU\\"",
 "nextPageToken": "GMaApprK59sCIJys1p7K59sC",
 "pollingIntervalMillis": 3000,
 "pageInfo": {
  "totalResults": 1,
  "resultsPerPage": 1
 },
 "items": [
  {
   "kind": "youtube#liveChatMessage",
   "etag": "\\"DuHzAJ-eQIiCIp7p4ldoVcVAOeY/PIDcEwPK1J_oA470Qxo2540iNZA\\"",
   "id": "LCC.CiMSIQoYVUNRSWRGX1lRS01iLTE5SG84bzVXYnhnEgUvbGl2ZRI5ChpDTWFBcHBySzU5c0NGUXh6Z3dvZGgzTURQZxIbQ1BlQXZaS3c1OXNDRmN3cEF3b2Q4NkVPNWcw",
   "snippet": {
    "type": "textMessageEvent",
    "liveChatId": "EiEKGFVDUUlkRl9ZUUtNYi0xOUhvOG81V2J4ZxIFL2xpdmU",
    "authorChannelId": "UCQIdF_YQKMb-19Ho8o5Wbxg",
    "publishedAt": "2018-06-22T15:25:11.777Z",
    "hasDisplayContent": true,
    "displayMessage": "hi",
    "textMessageDetails": {
     "messageText": "hi"
    }
   },
   "authorDetails": {
    "channelId": "UCQIdF_YQKMb-19Ho8o5Wbxg",
    "channelUrl": "http://www.youtube.com/channel/UCQIdF_YQKMb-19Ho8o5Wbxg",
    "displayName": "wondible",
    "profileImageUrl": "https://yt3.ggpht.com/-0Olxa2vkpR8/AAAAAAAAAAI/AAAAAAAAAAA/yYNHNpM1Htg/s88-c-k-no-mo-rj-c0xffffff/photo.jpg",
    "isVerified": false,
    "isChatOwner": true,
    "isChatSponsor": false,
    "isChatModerator": false
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

timeStamp : Decoder Posix
timeStamp =
  succeed (Time.millisToPosix 0)
  {-
  string
    |> andThen (\s -> case Date.fromString s of
      Ok d -> succeed (Date.toTime d)
      Err err -> fail err
    )
    -}
