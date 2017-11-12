module Model exposing (..)

import Maybe exposing (Maybe(..), withDefault)
import Time exposing (..)
import Dict
import Json.Decode as Decode
import Json.Encode as Encode


type alias Model =
    { activities : Dict.Dict String Activity
    , live : Timer
    , possibleName : String
    , message : String
    , haveSyncedOnce : Bool
    , accuracy : Bool
    }


type alias Activity =
    { budgeted : Int
    , spent : List Entry
    }


urgency : Activity -> Float
urgency activity =
    List.map duration activity.spent
        |> List.sum
        |> (-) (toFloat activity.budgeted)
        |> abs


type alias Entry =
    { start : Time
    , stop : Time
    , accurate : Bool
    }


type Timer
    = NoTimer
    | Open Time
    | Closed Time Time


duration : Entry -> Time
duration e =
    e.stop - e.start |> inMinutes


init =
    { activities = Dict.empty
    , live = NoTimer
    , possibleName = ""
    , message = ""
    , haveSyncedOnce = False
    , accuracy = True
    }


decodeEntry =
    Decode.map3 Entry
        (Decode.field "start" Decode.float)
        (Decode.field "stop" Decode.float)
        (Decode.andThen decodeAccurate (Decode.maybe (Decode.field "accurate" Decode.bool)))


decodeAccurate accurate =
    Decode.succeed <| withDefault True accurate


decodeActivity =
    Decode.map2 Activity
        (Decode.field "budgeted" Decode.int)
        (Decode.andThen decodeSpent (Decode.maybe (Decode.field "spent" (Decode.list decodeEntry))))


decodeSpent spent =
    Decode.succeed <| withDefault [] spent


decodeActivities =
    Decode.dict decodeActivity


encodeEntry : Entry -> Encode.Value
encodeEntry e =
    Encode.object
        [ ( "start", Encode.float e.start )
        , ( "stop", Encode.float e.stop )
        , ( "accurate", Encode.bool e.accurate )
        ]


encodeActivity a =
    Encode.object
        [ ( "budgeted", Encode.int a.budgeted )
        , ( "spent", Encode.list (List.map encodeEntry a.spent) )
        ]


encodeActivities a =
    Dict.toList a
        |> List.map (\( k, v ) -> ( k, encodeActivity v ))
        |> Encode.object
