module Main exposing (main)

import Model exposing (init, Model)
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
    = CategoryTyping String
    | NewCategory
    | NewTimer

update : Msg -> Model -> (Model, Cmd Msg)
update msg model=
    case msg of
        NewCategory ->
          model ! []

        CategoryTyping str ->
          {model | possibleName = str} ! []

        NewTimer ->
          model ! []


view model =
    div []
        [ topBar
        , liveBar model.live
--        , budgetBar model.weeks
        ]

topBar =
    div [ id "topBar" ]
        [ button [ onClick NewTimer ] [ text "Add Timer" ]
        , input [ onInput CategoryTyping] []
        , button [ onClick NewCategory ] [ text "Add Category" ]
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
