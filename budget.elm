module Main exposing (main)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task exposing (perform)
import Time exposing (..)
import Dict

main = program
      { init = init
      , view = view
      , update = update
      , subscriptions = (\_ -> Sub.none)
      }

type Msg
    = ActivityTyping String
    | NewActivity
    | CycleEventTimer
    | GotEventTimer String Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model=
    case msg of
        NewActivity ->
          let name = model.possibleName
          in
            if name == "" then model ! []
            else
              {model | activities = Dict.insert name (Activity 0 []) model.activities
                     , possibleName = ""
              } ! []
        ActivityTyping str ->
          {model | possibleName = str} ! []

        CycleEventTimer ->
          model ! [Task.perform (GotEventTimer "") now]

        GotEventTimer activity time ->
          case model.live of
            NoTimer -> {model | live = Open time} ! []
            Open start -> {model | live = Closed start time } ![]
            Closed _ _ -> {
              model | activities = Dict.update activity (ufunc model.live)  model.activities
                    , live = NoTimer
              } ! []

ufunc current x = case x of
  Nothing -> Nothing
  Just y -> Just <| Activity y.budgeted (current::y.spent)

view model =
    div [ id "topBar" ] <| [
     case model.live of
      NoTimer -> button [onClick CycleEventTimer ] [text "Start New Timer"]
      Open _ -> button [onClick CycleEventTimer ][text "Stop Timer"]
      Closed _ _ -> div []
        (Dict.keys model.activities
          |> List.map (\x -> button [onClick <| GotEventTimer x 0] [text x]))
    , input [ value model.possibleName, onInput ActivityTyping] []
    , button [ onClick NewActivity ] [ text "Add Activity" ]
    , div [] <| case model.live of
        Closed start stop -> [(stop - start)
                              |> inMinutes
                              |> toString
                              |> \x -> x ++ " minutes"
                              |> text]
        Open time -> [text (toString time)]
        NoTimer -> []
    ]
