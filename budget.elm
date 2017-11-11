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
import Json.Decode


main =
    program
        { init = ( init, fetchActivities )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.haveSyncedOnce then
        Time.every (30 * Time.second) SendActivities
    else
        Sub.none


type Msg
    = ActivityTyping String
    | NewActivity
    | CycleEventTimer
    | GotEventTimer String Time
    | Budget String String
    | SendActivities Time.Time
    | CommitActivities (Result Http.Error (Dict.Dict String Activity))
    | FetchActivities
    | ToggleAccuracy


url =
    "https://pebble-timetracking.firebaseio.com/activities.json"


fetchActivities =
    let
        request =
            Http.get url decodeActivities
    in
        Http.send CommitActivities request


sendActivities model =
    let
        request =
            Http.request
                { method = "PUT"
                , headers = []
                , url = "https://pebble-timetracking.firebaseio.com/activities.json"
                , body = Http.jsonBody (encodeActivities model.activities)
                , expect = Http.expectJson decodeActivities
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send CommitActivities request


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleAccuracy ->
          { model | accuracy = not model.accuracy } ! []
        FetchActivities ->
            { model | message = "Fetching Activities" } ! [ fetchActivities ]

        CommitActivities (Ok newActivities) ->
            { model
                | activities = newActivities
                , message = ""
                , haveSyncedOnce = True
            }
                ! []

        CommitActivities (Err e) ->
            case e of
                Http.Timeout ->
                    { model | message = "timeout" } ! []

                Http.NetworkError ->
                    { model | message = "network error" } ! []

                Http.BadUrl x ->
                    { model | message = "badurl:\n" ++ x } ! []

                Http.BadStatus x ->
                    { model | message = "badstatus:\n" ++ (toString x) } ! []

                Http.BadPayload x y ->
                    { model | message = "badpayload:\n" ++ x ++ "\n" ++ (toString y) } ! []

        SendActivities _ ->
            { model | message = "Sending Activities" } ! [ sendActivities model ]

        NewActivity ->
            let
                name =
                    model.possibleName

                new_model =
                    { model
                        | activities = Dict.insert name (Activity 0 []) model.activities
                        , possibleName = ""
                    }
            in
                if name == "" then
                    model ! []
                else
                    new_model ! [ sendActivities new_model ]

        ActivityTyping str ->
            { model
                | possibleName = str
            }
                ! []

        CycleEventTimer ->
            model ! [ Task.perform (GotEventTimer "") now ]

        GotEventTimer label time ->
            case model.live of
                NoTimer ->
                    { model | live = Open time } ! []

                Open start ->
                    { model | live = Closed start time } ! []

                Closed start stop ->
                    let
                        updateSpent x =
                            case x of
                                Nothing ->
                                    Nothing

                                Just y ->
                                    Just <| Activity y.budgeted ({ start= start
                                                                  , stop=stop
                                                                  , accurate=model.accuracy
                                                                  } :: y.spent)

                        new_model =
                            { model
                                | activities = Dict.update label updateSpent model.activities
                                , live = NoTimer
                                , accuracy = True
                            }
                    in
                        new_model ! [ sendActivities new_model ]

        Budget label unparsedAmount ->
            let
                amount =
                    String.toInt unparsedAmount |> Result.toMaybe |> Maybe.withDefault 0

                updateBudget x =
                    case x of
                        Nothing ->
                            Nothing

                        Just y ->
                            Just <| Activity amount y.spent

                new_model =
                    { model
                        | activities = Dict.update label updateBudget model.activities
                        , message = "Unsynced Changes"
                    }
            in
                new_model ! [ sendActivities new_model ]


view model =
    div [ id "topBar" ]
        [ div []
            [ div [] <|
                case model.live of
                    NoTimer ->
                        [ button [ onClick CycleEventTimer ] [ text "Start New Timer" ] ]

                    Open time ->
                        [ button [ onClick CycleEventTimer ] [ text "Stop Timer" ]
                        , text (toString time)
                        ]

                    Closed start stop ->
                        [ (stop - start)
                            |> inMinutes
                            |> toString
                            |> \x ->
                                x
                                    ++ " minutes"
                                    |> text
                        , label []
                          [ input [ type_ "checkbox"
                                  , checked model.accuracy
                                  , onClick ToggleAccuracy
                                  ] []
                          , text "Accurate"
                          ]
                        , span [] <|
                            (Dict.keys model.activities
                                |> List.map (\x -> button [ onClick <| GotEventTimer x 0 ] [ text x ])
                            )
                        ]
            ]
        , div [ id "list" ] (Dict.toList model.activities |> List.map activityRow)
        , input [ value model.possibleName, onInput ActivityTyping ] []
        , button [ onClick NewActivity ] [ text "Add Activity" ]
        , div []
            [ button [ onClick (SendActivities 0) ] [ text "Send Activities" ]
            , button [ onClick FetchActivities ] [ text "Fetch Activities" ]
            ]
        , div [] [ text model.message ]
        ]


onBlurWithTargetValue : (String -> msg) -> Attribute msg
onBlurWithTargetValue tagger =
    on "blur" (Json.Decode.map tagger targetValue)


activityRow x =
    let
        activityLabel =
            Tuple.first x
        activity =
            Tuple.second x
        kinda =
          if List.all .accurate activity.spent then "" else "~"
    in
        div []
            [ text activityLabel
            , input
                [ value <| toString activity.budgeted
                , onBlurWithTargetValue <| Budget activityLabel
                ]
                []
            , text <| kinda ++ (toString (List.sum (List.map duration activity.spent)))
            ]
