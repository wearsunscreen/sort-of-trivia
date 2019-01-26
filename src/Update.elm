module Update exposing (init, subscriptions, update)

import DnD exposing (Draggable, MousePosition)
import List exposing (filter, map)
import Maybe exposing (withDefault)
import Model exposing (..)
import Question exposing (Choice, allCategories, createQuestion, favoredCategory)
import Random exposing (Seed, initialSeed)
import Task exposing (Task, perform)
import Time exposing (now, posixToMillis)
import Tuple exposing (second)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { draggable = dnd.model
      , randomSeed = initialSeed 0
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

        Dropped potIndex choice ->
            let
                -- if the pot already has a choice, move it back to the unused choices
                opts =
                    map
                        (\c ->
                            if c.potIndex == potIndex then
                                { c | potIndex = -1 }

                            else
                                c
                        )
                        model.options
            in
            ( { model
                | options =
                    { choice | potIndex = potIndex } :: filter (\c -> c /= choice) opts
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
                , options = q
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
                , options = q
              }
            , Cmd.none
            )
