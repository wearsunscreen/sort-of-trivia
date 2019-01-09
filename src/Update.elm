module Update exposing (init, subs, update)

import DnD exposing (Draggable, MousePosition)
import Maybe exposing (withDefault)
import Model exposing (..)
import Random exposing (Seed, initialSeed)
import Task exposing (Task, perform)
import Time exposing (now, posixToMillis)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { draggable = dnd.model
      , randomSeed = Nothing
      , startTime = Nothing
      }
    , Cmd.none
    )


subs : Model -> Sub Msg
subs model =
    Sub.none


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

        StartApp time ->
            ( { model
                | startTime = Just time
                , randomSeed = Just (initialSeed (posixToMillis time))
              }
            , Cmd.none
            )
