import Monocle.Lens exposing (Lens)
import Maybe exposing (Maybe(..))

type alias Entry =
  { start : Time
  , stop : Time }

type alias Category =
  { budgeted : Int
  , spent : List Entry
  , name : String }

type alias Week =
  {categories: List Category}

type alias Interface =
  { live : Maybe Time
  , possibleName : Maybe String}

type alias Model =
  { weeks : List Week
  , ui : Interface
  }

init = ({weeks = [], ui = {live=Nothing, possibleName=Nothing} }, Cmd.none)
