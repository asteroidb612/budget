module Main exposing (main)

import Time.Date exposing (..)
import Time exposing (..)
import Task

main = program
      { init = init
      , view = view
      , update = update
      , subscriptions = (\_ -> Sub.none)
      }

type Msg
    = CategoryChange String
    | NewCategory
    | NewTimer

update : Msg -> Model -> (Model, Cmd Msg)
update msg model=
    case msg of
        NewCategory ->
          let
            newCat = model.ui.possibleName
          in
            {model | weeks = (Category 0 [] newCat) :: model.weeks} ! []

        CategoryChange str ->
          let
            u = model.ui
            newUI = {u | possibleName = str}
          in
            {model | ui = newUI} ! []

        NewTimer ->
          model ! []
