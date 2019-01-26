module Model exposing (Model, Msg(..), dnd)

import DnD
import Maybe exposing (Maybe)
import Question exposing (Choice)
import Random exposing (Seed)
import Time exposing (Posix)


type alias Draggable =
    DnD.Draggable Int Choice


type alias DraggableMsg =
    DnD.Msg Int Choice


type alias Model =
    { draggable : DnD.Draggable Int Choice
    , randomSeed : Seed
    , options : List Choice
    , startTime : Maybe Posix
    }


type Msg
    = CloseWelcomeScreen
    | Dropped Int Choice
    | DragMsg (DnD.Msg Int Choice)
    | NextQuestion
    | ResetQuestion
    | StartApp Posix
    | TestAnswers


dnd =
    DnD.init DragMsg Dropped
