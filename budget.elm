module Main exposing (main)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task exposing (perform)
import Time exposing (..)
import Dict
import Tuple

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
    div [ id "topBar" ]
    [ div []
      [ div [] <| case model.live of
        NoTimer ->
          [ button [onClick CycleEventTimer ] [text "Start New Timer"]]
        Open time ->
          [ button [onClick CycleEventTimer ] [text "Stop Timer"]
          , text (toString time)]
        Closed start stop ->
          [(stop - start)
            |> inMinutes
            |> toString
            |> \x -> x ++ " minutes"
            |> text
          , span [] <| (Dict.keys model.activities
              |> List.map (\x -> button [onClick <| GotEventTimer x 0] [text x]))
          ]
      ]
    , div [id "list"] (Dict.toList model.activities |> List.map activityRow )
    , input [ value model.possibleName, onInput ActivityTyping] []
    , button [ onClick NewActivity ] [ text "Add Activity" ]
    ]
activityRow x =
  let
    activityLabel = Tuple.first x
    activity = Tuple.second x
  in
    div []
      [ text activityLabel
      , input [activity.budgeted |> toString |> value] []
      , text (toString (List.sum(List.filterMap duration activity.spent)))
      ]
