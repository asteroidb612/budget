module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time.Date exposing (Date)
import Time exposing (Time)


main =
    Html.beginnerProgram { model = init, view = view, update = update }


type Msg
    = CategoryChange String
    | NewCategory
    | NewTimer


type alias Entry =
    { start : Time, end : Time, category : String }


type alias Week =
    { entries : List Entry, sunday : Date }


type alias Category =
    { weeks : List Week, name : String }


type alias Model =
    { weeks : List Entry, categories : List Category, possibleName : String }

emptyCategory x = {weeks = [], name = x}
init =
    { weeks = [], categories = [], possibleName = "" }


update : Msg -> Model -> Model
update msg model=
    case msg of
        NewCategory ->
            { model
                | categories = (emptyCategory model.possibleName) :: model.categories
                , possibleName = ""
            }

        CategoryChange str ->
            { model | possibleName = str }
        NewTimer ->
          model

view model =
    div []
        [ topBar
--        , liveBar
        , budgetBar model.categories
        ]


topBar =
    div [ id "topBar" ]
        [ button [ onClick NewTimer ] [ text "Add Timer" ]
        , input [ onInput CategoryChange ] []
        , button [ onClick NewCategory ] [ text "Add Category" ]
        ]


budgetBar cats =
    div [ class "budgetbar" ]
        [ div [ class "budgeted" ]
            [ text "Budgeted"
--            , List.map cats
            ]
        ]
