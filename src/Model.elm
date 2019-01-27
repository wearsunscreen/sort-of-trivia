module Model exposing (Mode(..), Model, Msg(..), dnd)

import DnD
import Maybe exposing (Maybe)
import Question exposing (Choice, Direction(..))
import Random exposing (Seed)
import Time exposing (Posix)


type alias Draggable =
    DnD.Draggable Direction Choice


type alias DraggableMsg =
    DnD.Msg Direction Choice


type Mode
    = Info
    | Play
    | Test


type alias Model =
    { draggable : DnD.Draggable Direction Choice
    , randomSeed : Seed
    , mode : Mode
    , options : List Choice
    , startTime : Maybe Posix
    }


type Msg
    = CloseWelcomeScreen
    | Dropped Direction Choice
    | DragMsg (DnD.Msg Direction Choice)
    | NextQuestion
    | ResetQuestion
    | StartApp Posix
    | TestAnswers


dnd =
    DnD.init DragMsg Dropped
