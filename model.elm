module Model exposing (..)

import Maybe exposing (Maybe(..))
import Time exposing (Time)
import Dict 

type alias Model =
  { activities: Dict.Dict String Activity
  , live : Entry
  , possibleName : String
  }

type alias Activity =
  { budgeted : Int
  , spent : List Entry
  }

type Entry = NoTimer | Open Time | Closed Time Time

init = (
         { activities= Dict.empty
         , live = NoTimer
         , possibleName = ""
         }
       , Cmd.none
       )
