module Main exposing (main)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task exposing (perform)
import Time exposing (..)
import Dict
import Tuple
import Http exposing (send, get, Error(..))


main = program
      { init = (init, fetchActivities)
      , view = view
      , update = update
      , subscriptions = subscriptions
      }
subscriptions : Model -> Sub Msg
subscriptions model =
  if model.haveSyncedOnce then Time.every (10*Time.second) SendActivities
                          else Sub.none


type Msg
    = ActivityTyping String
    | NewActivity
    | CycleEventTimer
    | GotEventTimer String Time
    | Budget String String
    | SendActivities Time.Time
    | CommitActivities (Result Http.Error (Dict.Dict String Activity))
    | FetchActivities

url =  "https://pebble-timetracking.firebaseio.com/activities.json"
request = Http.get url decodeActivities
fetchActivities = Http.send CommitActivities request

update : Msg -> Model -> (Model, Cmd Msg)
update msg model=
    case msg of
        FetchActivities ->
           {model | message = "Fetching Activities"} ! [fetchActivities]

        CommitActivities (Ok newActivities) ->
          {model | activities = newActivities
                 , message = ""
                 , haveSyncedOnce = True} ! []

        CommitActivities (Err e) -> case e of
          Http.Timeout -> {model | message = "timeout"} ! []
          Http.NetworkError -> {model | message = "network error"} ! []
          Http.BadUrl x -> {model | message = "badurl: " ++ x }  ! []
          Http.BadStatus x ->  {model | message = "badstatus: " ++ (toString x) } ! []
          Http.BadPayload x y -> {model | message = "badpayload: " ++ x ++ "\n" ++ (toString y)} ! []

        SendActivities _ ->
          let request = Http.request { method = "PUT"
                        , headers = []
                        , url = "https://pebble-timetracking.firebaseio.com/activities.json"
                        , body = Http.jsonBody (encodeActivities model.activities)
                        , expect = Http.expectJson decodeActivities
                        , timeout = Nothing
                        , withCredentials = False
                        }
          in {model | message="Sending Activities"} ! [Http.send CommitActivities request]

        NewActivity ->
          let name = model.possibleName
          in
            if name == "" then model ! []
            else
              {model | activities = Dict.insert name (Activity 0 []) model.activities
                     , possibleName = ""
                     } ! []
        ActivityTyping str ->
          {model | possibleName = str
                 , message = "Unsynced Changes"} ! []

        CycleEventTimer ->
          model ! [Task.perform (GotEventTimer "") now]

        GotEventTimer label time ->
          let
            updateSpent x = case x of
              Nothing -> Nothing
              Just y -> Just <| Activity y.budgeted (model.live::y.spent)
          in
            case model.live of
              NoTimer -> {model | live = Open time} ! []
              Open start -> {model | live = Closed start time } ![]
              Closed _ _ -> {
                model | activities = Dict.update label updateSpent model.activities
                      , live = NoTimer
                } ! []
        Budget label unparsedAmount->
            let
              amount = String.toInt unparsedAmount|> Result.toMaybe |> Maybe.withDefault 0
              updateBudget x = case x of
                Nothing -> Nothing
                Just y -> Just <| Activity amount y.spent
            in
              {model | activities = Dict.update label updateBudget model.activities
                     , message = "Unsynced Changes"} ! []

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
    , div []
      [ button [onClick (SendActivities 0)] [ text "Send Activities"]
      , button [onClick FetchActivities] [ text "Fetch Activities"]
      ]
    , div [] [text model.message]
    ]

activityRow x =
  let
    activityLabel = Tuple.first x
    activity = Tuple.second x
  in
    div []
      [ text activityLabel
      , input [ value <| toString activity.budgeted
              , onInput <| Budget activityLabel
              ] []
      , text (toString (List.sum(List.filterMap duration activity.spent)))
      ]
