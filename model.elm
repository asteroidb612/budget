module Model exposing (..)

import Maybe exposing (Maybe(..), withDefault)
import Time exposing (..)
import Dict
import Json.Decode as Decode
import Json.Encode as Encode

type alias Model =
  { activities: Dict.Dict String Activity
  , live : Entry
  , possibleName : String
  , message : String
  }

type alias Activity =
  { budgeted : Int
  , spent : List Entry
  }

type Entry = NoTimer | Open Time | Closed Time Time


duration : Entry -> Maybe Time
duration ent = case ent of
  NoTimer -> Nothing
  Open _ -> Nothing
  Closed start stop -> stop - start |> inMinutes |> Just

init =   { activities= Dict.empty
         , live = NoTimer
         , possibleName = ""
         , message = ""
         }


decodeEntry = Decode.map2 Closed (Decode.field "start" Decode.float) (Decode.field "stop" Decode.float)
decodeActivity = Decode.map2 Activity (Decode.field "budgeted" Decode.int)
                                      (Decode.andThen decodeSpent (Decode.maybe (Decode.field "spent" (Decode.list decodeEntry))))
decodeSpent spent = Decode.succeed <| withDefault [] spent

decodeActivities = Decode.dict decodeActivity

encodeEntry : Entry -> Encode.Value
encodeEntry e = case e of
  --This is bad. I have told this to turn wrong entries into impossible entries
  -- This indicates I may be thinking about the problem of cycling through timers wrong.
  -- Perhaps there is a type for a finished value which we make from an unfinished value?
  -- That unfinishedness should stay out of the logic operating on the finished
  NoTimer -> Encode.object [("start", Encode.float 0), ("stop", Encode.float 0)]
  Open t -> Encode.object [("start", Encode.float 0), ("stop", Encode.float t)]
  Closed t1 t2 -> Encode.object [("start", Encode.float t1), ("stop", Encode.float t2)]

encodeActivity a = Encode.object [("budgeted", Encode.int a.budgeted)
                                , ("spent", Encode.list (List.map encodeEntry a.spent))]

encodeActivities a = Dict.toList a
                        |> List.map (\(k,v) -> (k, encodeActivity v))
                        |> Encode.object
