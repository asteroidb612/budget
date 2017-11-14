module Budget exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (Maybe(..), withDefault)
import Task exposing (perform)
import Time exposing (..)
import Dict
import Tuple
import Http exposing (send, get, Error(..))
import Json.Decode as Decode
import Json.Encode as Encode


type alias Entry =
    { start : Time
    , stop : Time
    , accurate : Bool
    }


duration : Entry -> Time
duration e =
    e.stop - e.start |> inMinutes


type alias Activity =
    { budgeted : Int
    , spent : List Entry
    }


urgency : Activity -> Float
urgency activity =
    List.map duration activity.spent
        |> List.sum
        |> (-) (toFloat activity.budgeted)
        |> abs


type Timer
    = NoTimer
    | Open Time
    | Closed Time Time


type alias Model =
    { activities : Dict.Dict String Activity
    , live : Timer
    , possibleName : String
    , message : String
    , haveSyncedOnce : Bool
    , accuracy : Bool
    }


init =
    { activities = Dict.empty
    , live = NoTimer
    , possibleName = ""
    , message = ""
    , haveSyncedOnce = False
    , accuracy = True
    }


decodeLive =
    Decode.nullable Decode.float


decodeEntry =
    Decode.map3 Entry
        (Decode.field "start" Decode.float)
        (Decode.field "stop" Decode.float)
        (Decode.andThen decodeAccurate (Decode.maybe (Decode.field "accurate" Decode.bool)))


decodeAccurate accurate =
    Decode.succeed <| withDefault True accurate


decodeActivity =
    Decode.map2 Activity
        (Decode.field "budgeted" Decode.int)
        (Decode.andThen decodeSpent (Decode.maybe (Decode.field "spent" (Decode.list decodeEntry))))


decodeSpent spent =
    Decode.succeed <| withDefault [] spent


decodeActivities =
    Decode.dict decodeActivity


encodeLive : Time.Time -> Encode.Value
encodeLive =
    Encode.float


encodeEntry : Entry -> Encode.Value
encodeEntry e =
    Encode.object
        [ ( "start", Encode.float e.start )
        , ( "stop", Encode.float e.stop )
        , ( "accurate", Encode.bool e.accurate )
        ]


encodeActivity a =
    Encode.object
        [ ( "budgeted", Encode.int a.budgeted )
        , ( "spent", Encode.list (List.map encodeEntry a.spent) )
        ]


encodeActivities a =
    Dict.toList a
        |> List.map (\( k, v ) -> ( k, encodeActivity v ))
        |> Encode.object


main =
    program
        { init = init ! [ fetchActivities, fetchLive ]
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type ServerReaction
    = Save
    | NewLeaf


type Msg
    = ActivityTyping String
    | NewActivity
    | CycleEventTimer
    | GotEventTimer String Time
    | Budget String String
    | CommitActivities ServerReaction (Result Http.Error (Dict.Dict String Activity))
    | ToggleAccuracy
    | TurnNewLeaf
    | SendNewLeaf Time.Time
    | Discard
    | CommitLive (Result Http.Error (Maybe Float))


urlBase =
    "https://pebble-timetracking.firebaseio.com/"


putRequest model destination =
    Http.request
        { method = "PUT"
        , headers = []
        , url = urlBase ++ destination ++ ".json"
        , body = Http.jsonBody (encodeActivities model.activities)
        , expect = Http.expectJson decodeActivities
        , timeout = Nothing
        , withCredentials = False
        }


sendActivities model =
    Http.send (CommitActivities Save) <|
        putRequest model "activities"


fetchActivities =
    Http.send (CommitActivities Save) <|
        Http.get (urlBase ++ "activities.json") decodeActivities


fetchLive =
    Http.send (CommitLive) <|
        Http.get (urlBase ++ "live.json") decodeLive


handleError model e =
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CommitLive (Ok t) ->
            case t of
                Nothing ->
                    model ! []

                Just t ->
                    { model | live = Open t } ! []

        CommitActivities behavior (Ok newActivities) ->
            case behavior of
                NewLeaf ->
                    let
                        clearout name activity =
                            { activity
                                | budgeted = 0
                                , spent = []
                            }

                        resetActivities =
                            Dict.map clearout newActivities
                    in
                        { model
                            | message = ""
                            , activities = resetActivities
                        }
                            ! []

                Save ->
                    { model
                        | activities = newActivities
                        , message = ""
                        , haveSyncedOnce = True
                    }
                        ! []

        CommitLive (Err e) ->
            handleError model e

        CommitActivities _ (Err e) ->
            handleError model e

        Discard ->
            { model
                | live = NoTimer
                , message = "Timer Deleted"
            }
                ! []

        TurnNewLeaf ->
            { model | message = "Turning New Leaf" } ! [ Task.perform SendNewLeaf now ]

        SendNewLeaf time ->
            let
                name =
                    "new-leaf-" ++ (toString time)

                sendNewLeaf =
                    Http.send (CommitActivities NewLeaf) <| putRequest model name
            in
                { model | message = "Sending New Leaf" } ! [ sendNewLeaf ]

        ToggleAccuracy ->
            { model | accuracy = not model.accuracy } ! []

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
                    let
                        request =
                            Http.request
                                { method = "PUT"
                                , headers = []
                                , url = urlBase ++ "live.json"
                                , body = Http.jsonBody (encodeLive time)
                                , expect = Http.expectJson decodeLive
                                , timeout = Nothing
                                , withCredentials = False
                                }
                    in
                        { model | live = Open time }
                            ! [ Http.send CommitLive request ]

                Open start ->
                    { model | live = Closed start time } ! []

                Closed start stop ->
                    let
                        updateSpent x =
                            case x of
                                Nothing ->
                                    Nothing

                                Just y ->
                                    Just <|
                                        Activity y.budgeted
                                            ({ start = start
                                             , stop = stop
                                             , accurate = model.accuracy
                                             }
                                                :: y.spent
                                            )

                        new_model =
                            { model
                                | activities = Dict.update label updateSpent model.activities
                                , live = NoTimer
                                , accuracy = True
                            }
                    in
                        new_model
                            ! [ sendActivities new_model
                              , Http.send CommitLive <|
                                    Http.request
                                        { method = "PUT"
                                        , headers = []
                                        , url = urlBase ++ "live.json"
                                        , body = Http.jsonBody Encode.null
                                        , expect = Http.expectJson decodeLive
                                        , timeout = Nothing
                                        , withCredentials = False
                                        }
                              ]

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
    if model.haveSyncedOnce then
        (visible model)
    else
        div [] []


visible model =
    div [ id "topBar" ]
        [ div []
            [ div [] <|
                case model.live of
                    NoTimer ->
                        [ button [ onClick CycleEventTimer ] [ text "Start New Timer" ]
                        ]

                    Open time ->
                        [ button [ onClick CycleEventTimer ] [ text "Stop Timer" ]
                        , text (toString time)
                        , div [] [ button [ onClick Discard ] [ text "Discard" ] ]
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
                            [ input
                                [ type_ "checkbox"
                                , checked model.accuracy
                                , onClick ToggleAccuracy
                                ]
                                []
                            , text "Accurate"
                            ]
                        , Dict.keys model.activities
                            |> List.map
                                (\x ->
                                    button [ onClick (GotEventTimer x 0) ] [ text x ]
                                )
                            |> div []
                        , div [] [ button [ onClick Discard ] [ text "Discard" ] ]
                        ]
            ]
        , table [ id "list" ]
            (Dict.toList model.activities
                |> List.sortBy (\activityPair -> urgency (Tuple.second activityPair))
                |> List.map activityRow
                |> List.reverse
            )
        , input [ value model.possibleName, onInput ActivityTyping ] []
        , button [ onClick NewActivity ] [ text "Add Activity" ]
        , div [] [ button [ onClick TurnNewLeaf ] [ text "Turn New Leaf" ] ]
        , div [] [ text model.message ]
        ]


onBlurWithTargetValue : (String -> msg) -> Attribute msg
onBlurWithTargetValue tagger =
    on "blur" (Decode.map tagger targetValue)


activityRow x =
    let
        activityLabel =
            Tuple.first x

        activity =
            Tuple.second x

        kinda =
            if List.all .accurate activity.spent then
                ""
            else
                "~"
    in
        tr []
            [ td [] [ text activityLabel ]
            , td []
                [ input
                    [ value <| toString activity.budgeted
                    , onBlurWithTargetValue <| Budget activityLabel
                    ]
                    []
                ]
            , td [] [ text <| kinda ++ (toString (List.sum (List.map duration activity.spent))) ]
            ]
