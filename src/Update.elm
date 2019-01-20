module Update exposing (init, subscriptions, update)

import DnD exposing (Draggable, MousePosition)
import Maybe exposing (withDefault)
import Model exposing (..)
import Question exposing (Question, allCategories, createQuestion, favoredCategory)
import Random exposing (Seed, initialSeed)
import Task exposing (Task, perform)
import Time exposing (now, posixToMillis)
import Tuple exposing (second)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { draggable = dnd.model
      , randomSeed = initialSeed 0
      , pots = []
      , question = createQuestion (initialSeed 2) [ favoredCategory ] |> second
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

        DropToGrid item ->
            ( model, Cmd.none )

        NextQuestion ->
            let
                ( s, q ) =
                    createQuestion model.randomSeed []
            in
            ( { model
                | randomSeed = s
                , question = q
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
                , question = q
              }
            , Cmd.none
            )
