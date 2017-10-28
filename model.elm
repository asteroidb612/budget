module Model exposing (..)

import Maybe exposing (Maybe(..), withDefault)
import Time exposing (..)
import Dict
import Json.Decode as Decode
import Json.Encode as Encode

type alias Model =
  { activities: Dict.Dict String Activity
  , live : Timer
  , possibleName : String
  , message : String
  , haveSyncedOnce : Bool
  }

type alias Activity =
  { budgeted : Int
  , spent : List Entry
  }

type alias Entry = (Time, Time)
type Timer = NoTimer | Open Time | Closed Time Time

duration : Entry -> Time
duration (start, stop) = stop - start |> inMinutes

init =   { activities= Dict.empty
         , live = NoTimer
         , possibleName = ""
         , message = ""
         , haveSyncedOnce = False
         }

decodeEntry = Decode.map2 (,) (Decode.field "start" Decode.float) (Decode.field "stop" Decode.float)
decodeActivity = Decode.map2 Activity (Decode.field "budgeted" Decode.int)
                                      (Decode.andThen decodeSpent (Decode.maybe (Decode.field "spent" (Decode.list decodeEntry))))
decodeSpent spent = Decode.succeed <| withDefault [] spent

decodeActivities = Decode.dict decodeActivity

encodeEntry : Entry -> Encode.Value
encodeEntry (start, stop) = Encode.object [("start", Encode.float start), ("stop", Encode.float stop)]

encodeActivity a = Encode.object [("budgeted", Encode.int a.budgeted)
                                , ("spent", Encode.list (List.map encodeEntry a.spent))]

encodeActivities a = Dict.toList a
                        |> List.map (\(k,v) -> (k, encodeActivity v))
                        |> Encode.object
