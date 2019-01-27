module Update exposing (init, subscriptions, update)

import DnD exposing (Draggable, MousePosition)
import List exposing (filter, map)
import Maybe exposing (withDefault)
import Model exposing (..)
import Question exposing (Choice, Direction(..), allCategories, createQuestion, favoredCategory)
import Random exposing (Seed, initialSeed)
import Task exposing (Task, perform)
import Time exposing (now, posixToMillis)
import Tuple exposing (second)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { draggable = dnd.model
      , randomSeed = initialSeed 0
      , mode = Info
      , options =
            createQuestion (initialSeed 2) [ favoredCategory ]
                |> second
      , startTime = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dnd.subscriptions model.draggable
        ]


{-| If an choice in in the wrong pot return it to stack
-}
testOption : Choice -> Choice
testOption opt =
    opt


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        CloseWelcomeScreen ->
            ( model, Task.perform StartApp Time.now )

        DragMsg msg ->
            ( { model
                | draggable = DnD.update msg model.draggable
              }
            , Cmd.none
            )

        Dropped potDir choice ->
            let
                -- if the pot already has a choice, move it back to the unused choices
                opts =
                    map
                        (\c ->
                            if c.potDirection == potDir then
                                { c | potDirection = Lost }

                            else
                                c
                        )
                        model.options
            in
            ( { model
                | options =
                    { choice | potDirection = potDir } :: filter (\c -> c /= choice) opts
                , mode = Play
              }
            , Cmd.none
            )

        NextQuestion ->
            let
                ( s, q ) =
                    createQuestion model.randomSeed []
            in
            ( { model
                | randomSeed = s
                , mode = Play
                , options = q
              }
            , Cmd.none
            )

        ResetQuestion ->
            ( { model
                | options = map (\opt -> { opt | potDirection = Lost }) model.options
                , mode = Play
              }
            , Cmd.none
            )

        StartApp time ->
            let
                ( s, q ) =
                    createQuestion (initialSeed (posixToMillis time)) []
            in
            ( { model
                | startTime = Just time
                , randomSeed = s
                , mode = Play
                , options = q
              }
            , Cmd.none
            )

        TestAnswers ->
            ( { model
                | mode = Test
                , options = map (\opt -> testOption opt) model.options
              }
            , Cmd.none
            )
