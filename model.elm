module Model exposing (..)

import Maybe exposing (Maybe(..))
import Time exposing (Time)

type alias Model =
  { activities: List Activity
  , live : Entry
  , possibleName : String
  }


type alias Activity =
  { budgeted : Int
  , spent : List Entry
  , name : String }

spent : Activity -> Float
spent a = List.filterMap duration a.spent |>  List.sum


type Entry = NoTimer | Open Time | Closed Time Time

duration : Entry -> Maybe Time
duration e =
  case e of
    Closed start stop ->  stop - start |> Just
    Open _ -> Nothing
    NoTimer -> Nothing


init = (
         { activities= []
         , live=NoTimer
         , possibleName=""
         }
       , Cmd.none
       )
