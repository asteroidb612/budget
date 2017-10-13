module Model exposing (..)

import Maybe exposing (Maybe(..))
import Time exposing (..)
import Dict
import Json.Decode exposing (..)

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

init = (
         { activities= Dict.empty
         , live = NoTimer
         , possibleName = ""
         , message = ""
         }
       , Cmd.none
       )

entry = map2 Closed (field "start" float) (field "end" float)
activity = map2 Activity (field "budgeted" int) (field "spent" (list entry))
