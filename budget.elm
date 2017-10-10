module Main exposing (main)

import Model exposing (init, Model, Activity)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

main = program
      { init = init
      , view = view
      , update = update
      , subscriptions = (\_ -> Sub.none)
      }

type Msg
    = ActivityTyping String
    | NewActivity
    | FetchTimer

update : Msg -> Model -> (Model, Cmd Msg)
update msg model=
    case msg of
        NewActivity ->
          let name = model.possibleName
          in {model | activities = (Activity 0 [] name):: model.activities
                    , possibleName = ""
             } ! []
        ActivityTyping str ->
          {model | possibleName = str} ! []

        FetchTimer->
          model ! []


view model =
    div []
        [ topBar model.possibleName
        , liveBar model.live
--        , budgetBar model.weeks
        ]

topBar possibleName =
    div [ id "topBar" ]
        [ button [ onClick FetchTimer] [ text "Add Timer" ]
        , input [ value possibleName, onInput ActivityTyping] []
        , button [ onClick NewActivity ] [ text "Add Activity" ]
        ]

liveBar timer =
  case timer of
      Nothing -> div [] []
      Just time -> div [] [text (toString time)]

budgetBar cats =
    div [ class "budgetbar" ]
        [ div [ class "budgeted" ]
            [ text "Budgeted"
--            , List.map cats
            ]
        ]
