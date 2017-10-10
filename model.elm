module Model exposing (init, Model)

import Maybe exposing (Maybe(..))
import Time exposing (Time)

type Entry = Open Time | Closed Time Time

duration : Entry -> Maybe Time
duration e =
  case e of
    Closed start stop ->  stop - start |> Just
    Open _ -> Nothing

type alias Activity =
  { budgeted : Int
  , spent : List Entry
  , name : String }

spent : Activity -> Float
spent a = List.filterMap duration a.spent |>  List.sum

type alias Model =
  { activities: List Activity
  , live : Maybe Time
  , possibleName : String
  }

init = (
         { activities= []
         , live=Nothing
         , possibleName=""
         }
       , Cmd.none
       )
